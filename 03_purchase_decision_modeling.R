# ══════════════════════════════════════════════════════════════════════════════
# 03 — Purchase Decision Modeling
# ══════════════════════════════════════════════════════════════════════════════
#
# PURPOSE:
#   Model whether a website visitor will purchase a ticket, and quantify the
#   effect of dynamic pricing on conversion. This is the analytical culmination
#   of the pipeline: raw audit logs → temporal snapshots → this model.
#
# APPROACH:
#   1. Feature engineering: visitor history (leak-free), session context, timing
#   2. Visitor segmentation: never-purchased vs. prior purchasers
#   3. XGBoost + Keras ensemble models across multiple feature sets
#   4. Calibrated partial dependence plots (Platt scaling)
#   5. Game tier segmentation: high vs. low demand
#   6. K-means visitor clustering → per-cluster conversion models
#
# INPUT:
#   data/visit_event_summary.csv — one row per visit × game
#   data/event_scores.csv        — DA game demand scores
#   data/grandstand_orders.csv   — ticket order fallback for missing seat info
#
# OUTPUT:
#   Feature importance plots, partial dependence plots, cluster bubble chart,
#   probability distribution histograms, model diagnostics
#
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(xgboost)
library(pdp)
library(pROC)
library(patchwork)
library(cluster)
library(factoextra)
library(ggrepel)

# Keras requires a Python + TensorFlow backend. If unavailable, the pipeline
# falls back to XGBoost-only (no neural network ensemble component).
keras_available <- tryCatch({
  library(keras)
  is_keras_available()
}, error = function(e) FALSE)

if (!keras_available) {
  cat("NOTE: Keras/TensorFlow not available — neural network models will be skipped.\n",
      "      XGBoost results are unaffected.\n")
}

# ── HELPER: aligned double matrix for xgboost ───────────────────────────────
# xgb.DMatrix requires 64-byte-aligned memory; subsetting a matrix in R can
# produce a misaligned view.  Forcing storage.mode to "double" triggers a copy.
aligned_dmatrix <- function(x, ...) {
  m <- as.matrix(x)
  storage.mode(m) <- "double"
  xgb.DMatrix(m, ...)
}

# ── LOAD DATA ────────────────────────────────────────────────────────────────

visits <- read.csv("data/visit_event_summary.csv", stringsAsFactors = FALSE) %>%
  mutate(
    event_date          = as.Date(event_date),
    first_hit_timestamp = as.POSIXct(first_hit_timestamp, tz = "America/New_York"),
    purchased           = as.logical(purchased),
    days_before_game    = as.integer(event_date - as.Date(first_hit_timestamp)),
    section             = as.character(section),
    row                 = as.character(row)
  )

cat("Total visits:", nrow(visits), "\n")
cat("Purchased:   ", sum(visits$purchased, na.rm = TRUE), "\n")
cat("Conversion:  ", scales::percent(mean(visits$purchased, na.rm = TRUE), accuracy = 0.01), "\n")

# ── RECOVER MISSING SEAT INFO VIA GRANDSTAND FALLBACK ────────────────────────

grandstand_orders <- read.csv("data/grandstand_orders.csv", stringsAsFactors = FALSE) %>%
  mutate(event_date = as.Date(event_date),
         section    = as.character(section),
         row        = as.character(row))

grandstand_fallback <- grandstand_orders %>%
  filter(!is.na(price_location)) %>%
  group_by(order_key) %>%
  slice(1) %>%
  ungroup()

missing_keys <- visits %>%
  filter(!is.na(purchase_id), is.na(section)) %>%
  mutate(
    order_key = case_when(
      str_detect(purchase_id, "/") ~ str_extract(purchase_id, "(?<=ATL:TM_)[^/]+"),
      TRUE                         ~ str_extract(purchase_id, "(?<=ATL:TM_)\\d+")
    )
  ) %>%
  left_join(grandstand_fallback, by = "order_key") %>%
  select(visit_id, event_name,
         section        = section.y,
         row            = row.y,
         price_location = price_location.y,
         num_seats      = num_seats.y,
         ticket_price   = ticket_price.y,
         total_price    = total_price.y)

visits <- visits %>%
  left_join(missing_keys, by = c("visit_id", "event_name"), suffix = c("", "_fallback")) %>%
  mutate(
    section        = coalesce(section,        section_fallback),
    row            = coalesce(row,            row_fallback),
    price_location = coalesce(price_location, price_location_fallback),
    num_seats      = coalesce(num_seats,      num_seats_fallback),
    ticket_price   = coalesce(ticket_price,   ticket_price_fallback),
    total_price    = coalesce(total_price,    total_price_fallback)
  ) %>%
  select(-ends_with("_fallback"))

visits %>%
  filter(!is.na(purchase_id)) %>%
  summarise(
    total_purchases   = n(),
    missing_seat_info = sum(is.na(section))
  )

# ── EVENT SCORES ─────────────────────────────────────────────────────────────

da_event_scores <- read.csv("data/event_scores.csv", stringsAsFactors = FALSE) %>%
  select(EVENT, DATETIME, EVENTSCORE) %>%
  mutate(event_date = as.Date(str_trim(DATETIME)))

visits <- visits %>%
  left_join(da_event_scores %>% select(event_date, EVENT, EVENTSCORE), by = "event_date")

# ── VISITOR-LEVEL FEATURE ENGINEERING (leak-free) ────────────────────────────
# All history features use lag() to ensure we only look at PRIOR visits,
# preventing data leakage from future behavior into the prediction.

visits_ordered <- visits %>%
  mutate(purchased_int = as.integer(purchased)) %>%
  arrange(visitor_id, first_hit_timestamp)

visitor_history <- visits_ordered %>%
  group_by(visitor_id) %>%
  mutate(
    visitor_ever_purchased     = lag(cumsum(purchased_int), default = 0) > 0,
    visitor_n_purchases        = lag(cumsum(purchased_int), default = 0),
    visitor_total_games_viewed = lag(row_number(),          default = 0),
    visitor_total_visits       = lag(cumsum(!duplicated(visit_id)), default = 0)
  ) %>%
  ungroup() %>%
  select(visitor_id, visit_id, event_name, first_hit_timestamp,
         visitor_ever_purchased, visitor_n_purchases,
         visitor_total_games_viewed, visitor_total_visits)

session_features <- visits %>%
  group_by(visit_id) %>%
  summarise(session_games_viewed = n(), .groups = "drop")

repeat_views <- visits_ordered %>%
  group_by(visitor_id, event_name) %>%
  mutate(
    n_times_viewed_game = lag(row_number(), default = 0),
    n_days_viewed_game  = lag(cumsum(!duplicated(as.Date(first_hit_timestamp))), default = 0)
  ) %>%
  ungroup() %>%
  select(visitor_id, visit_id, event_name, first_hit_timestamp,
         n_times_viewed_game, n_days_viewed_game)

# ── BUILD MODEL DATA (2+ game viewers) ──────────────────────────────────────

model_df <- visits %>%
  mutate(
    hour_of_day = hour(first_hit_timestamp),
    day_of_week = wday(first_hit_timestamp, label = TRUE),
    is_weekend  = day_of_week %in% c("Sat", "Sun"),
    purchased   = as.integer(purchased)
  ) %>%
  group_by(visit_id) %>%
  filter(n() >= 2) %>%
  ungroup() %>%
  filter(
    days_before_game >= 0,
    !is.na(EVENTSCORE),
    !is.na(cheapest_price),
    !is.na(dist_open_count),
    !is.na(days_before_game)
  ) %>%
  left_join(visitor_history,  by = c("visitor_id", "visit_id", "event_name", "first_hit_timestamp")) %>%
  left_join(session_features, by = "visit_id") %>%
  left_join(repeat_views,     by = c("visitor_id", "visit_id", "event_name", "first_hit_timestamp"))

cat("2+ game viewers — Rows:", nrow(model_df),
    "| Purchase rate:", scales::percent(mean(model_df$purchased), accuracy = 0.01), "\n")

# ── BUILD MODEL DATA (all visitors) ─────────────────────────────────────────

model_df_all <- visits %>%
  mutate(
    hour_of_day = hour(first_hit_timestamp),
    day_of_week = wday(first_hit_timestamp, label = TRUE),
    is_weekend  = day_of_week %in% c("Sat", "Sun"),
    purchased   = as.integer(purchased)
  ) %>%
  filter(
    days_before_game >= 0,
    !is.na(EVENTSCORE),
    !is.na(cheapest_price),
    !is.na(dist_open_count),
    !is.na(days_before_game)
  ) %>%
  left_join(visitor_history,  by = c("visitor_id", "visit_id", "event_name", "first_hit_timestamp")) %>%
  left_join(session_features, by = "visit_id") %>%
  left_join(repeat_views,     by = c("visitor_id", "visit_id", "event_name", "first_hit_timestamp"))

cat("All visitors — Rows:", nrow(model_df_all),
    "| Purchase rate:", scales::percent(mean(model_df_all$purchased), accuracy = 0.01), "\n")

# ── FEATURE SETS ─────────────────────────────────────────────────────────────

features_all_visitors <- c(
  "days_before_game", "dist_open_count", "cheapest_price", "EVENTSCORE",
  "hour_of_day", "visitor_total_visits", "visitor_total_games_viewed",
  "visitor_ever_purchased", "visitor_n_purchases", "session_games_viewed",
  "n_times_viewed_game", "n_days_viewed_game"
)

features_new <- c(
  "days_before_game", "dist_open_count", "cheapest_price", "EVENTSCORE",
  "hour_of_day", "session_games_viewed", "n_times_viewed_game", "n_days_viewed_game"
)

features_game_only <- c(
  "days_before_game", "dist_open_count", "cheapest_price", "EVENTSCORE", "hour_of_day"
)

features_game_only_avgsub <- c(
  "days_before_game", "dist_open_count", "avg_price", "EVENTSCORE", "hour_of_day"
)

features_with_all_prices <- c(
  "days_before_game", "dist_open_count", "cheapest_price", "avg_price",
  "priciest_price", "EVENTSCORE", "hour_of_day"
)

# ── SPLIT SEGMENTS ───────────────────────────────────────────────────────────

df_new    <- model_df %>% filter(!visitor_ever_purchased)
df_repeat <- model_df %>% filter(visitor_ever_purchased)

df_all_new    <- model_df_all %>% filter(!visitor_ever_purchased)
df_all_repeat <- model_df_all %>% filter(visitor_ever_purchased)

cat("2+ games — Never-purchased:", nrow(df_new),
    "| Purchase rate:", scales::percent(mean(df_new$purchased), accuracy = 0.01), "\n")
cat("2+ games — Prior purchasers:", nrow(df_repeat),
    "| Purchase rate:", scales::percent(mean(df_repeat$purchased), accuracy = 0.01), "\n")
cat("All — Never-purchased:", nrow(df_all_new),
    "| Purchase rate:", scales::percent(mean(df_all_new$purchased), accuracy = 0.01), "\n")
cat("All — Prior purchasers:", nrow(df_all_repeat),
    "| Purchase rate:", scales::percent(mean(df_all_repeat$purchased), accuracy = 0.01), "\n")

# ── MODEL HELPER FUNCTION ───────────────────────────────────────────────────
# Trains XGBoost (and optionally Keras), returns scored data + model object.

run_models <- function(df, features, label, use_keras = TRUE) {

  # Guard: skip if too few rows to train

  if (nrow(df) < 10) {
    cat("\n── Skipping", label, "— only", nrow(df), "rows (need >= 10) ──\n")
    return(list(
      data = df %>% mutate(prob_xgb = NA_real_, prob_nn = NA_real_,
                           prob_ensemble = NA_real_, segment = label),
      xgb_model = NULL
    ))
  }

  X <- df %>%
    select(all_of(features)) %>%
    mutate(across(where(is.logical), as.integer)) %>%
    as.matrix()
  storage.mode(X) <- "double"   # force aligned copy (avoids xgb pointer misalignment)

  y <- as.integer(df$purchased)

  set.seed(42)
  train_idx <- sample(nrow(df), 0.8 * nrow(df))

  X_train <- X[train_idx, , drop = FALSE];  X_test <- X[-train_idx, , drop = FALSE]
  y_train <- y[train_idx];                  y_test <- y[-train_idx]

  dtrain <- aligned_dmatrix(X_train, label = y_train)
  dtest  <- aligned_dmatrix(X_test,  label = y_test)

  xgb_model <- xgb.train(
    params = list(
      objective        = "binary:logistic",
      eval_metric      = "auc",
      max_depth        = 4,
      eta              = 0.05,
      subsample        = 0.8,
      colsample_bytree = 0.8,
      scale_pos_weight = sum(y_train == 0) / sum(y_train == 1)
    ),
    data                  = dtrain,
    nrounds               = 300,
    evals                 = list(train = dtrain, test = dtest),
    early_stopping_rounds = 20,
    verbose               = 1
  )

  test_preds <- predict(xgb_model, dtest)
  test_auc   <- as.numeric(pROC::auc(pROC::roc(y_test, test_preds, quiet = TRUE)))

  cat("\n──", label, "XGBoost importance ──\n")
  imp <- xgb.importance(model = xgb_model)
  print(imp)

  p_imp <- imp %>%
    mutate(Feature = reorder(Feature, Gain)) %>%
    ggplot(aes(x = Gain, y = Feature)) +
    geom_col(fill = "#1565C0", alpha = 0.8) +
    geom_text(aes(label = scales::percent(Gain, accuracy = 0.1)),
              hjust = -0.1, size = 3.5) +
    scale_x_continuous(expand = expansion(mult = c(0, 0.15)),
                       labels = scales::percent) +
    labs(
      title    = paste(label, "— XGBoost Feature Importance"),
      subtitle = paste("Test AUC:", round(as.numeric(test_auc), 4)),
      x = "Gain", y = NULL
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.subtitle = element_text(color = "#B71C1C", face = "bold"))
  print(p_imp)

  prob_xgb <- predict(xgb_model, aligned_dmatrix(X))

  if (use_keras && keras_available) {
    prob_nn <- tryCatch({
      X_scaled   <- scale(X)
      X_train_sc <- X_scaled[train_idx, ]
      X_test_sc  <- X_scaled[-train_idx, ]

      model_nn <- keras_model_sequential() %>%
        layer_dense(units = 32, activation = "relu", input_shape = ncol(X_train_sc)) %>%
        layer_dropout(rate = 0.2) %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_dense(units = 1,  activation = "sigmoid")

      model_nn %>% compile(
        optimizer = optimizer_adam(learning_rate = 0.01),
        loss      = "binary_crossentropy",
        metrics   = list(metric_auc())
      )

      history <- model_nn %>% fit(
        X_train_sc, y_train,
        epochs          = 50,
        batch_size      = 256,
        validation_data = list(X_test_sc, y_test),
        callbacks       = list(
          callback_early_stopping(patience = 5, restore_best_weights = TRUE)
        ),
        verbose = 0
      )

      cat("\n──", label, "Keras evaluation ──\n")
      print(model_nn %>% evaluate(X_test_sc, y_test))
      plot(history)

      as.vector(model_nn %>% predict(X_scaled))
    }, error = function(e) {
      cat("\n  Keras failed for", label, "— falling back to XGBoost-only:", conditionMessage(e), "\n")
      rep(NA_real_, nrow(df))
    })
  } else {
    prob_nn <- rep(NA_real_, nrow(df))
  }

  list(
    data = df %>%
      mutate(
        prob_xgb      = prob_xgb,
        prob_nn       = prob_nn,
        prob_ensemble = ifelse(is.na(prob_nn), prob_xgb, (prob_xgb + prob_nn) / 2),
        segment       = label
      ),
    xgb_model = xgb_model
  )
}

# ── RUN MODELS (2+ game viewers) ────────────────────────────────────────────

out_new       <- run_models(df_new,    features_new,          "Never-purchased (2+ games)", use_keras = TRUE)
out_repeat    <- run_models(df_repeat, features_all_visitors, "Prior purchasers (2+ games)", use_keras = FALSE)
out_game_only <- run_models(model_df,  features_game_only,    "Game-only (2+ games)",        use_keras = FALSE)
out_game_only_avg_sub <- run_models(model_df, features_game_only_avgsub, "Game-only avg-price (2+ games)", use_keras = FALSE)

results_new       <- out_new$data
results_repeat    <- out_repeat$data
results_game_only <- out_game_only$data

xgb_model_new    <- out_new$xgb_model
xgb_model_repeat <- out_repeat$xgb_model
xgb_model_game   <- out_game_only$xgb_model

model_df_scored <- bind_rows(results_new, results_repeat)

# ── RUN MODELS (all visitors) ───────────────────────────────────────────────

out_all_new    <- run_models(df_all_new,    features_new,          "Never-purchased (all)", use_keras = FALSE)
out_all_repeat <- run_models(df_all_repeat, features_all_visitors, "Prior purchasers (all)", use_keras = FALSE)

results_all_new    <- out_all_new$data
results_all_repeat <- out_all_repeat$data

xgb_model_all_new    <- out_all_new$xgb_model
xgb_model_all_repeat <- out_all_repeat$xgb_model

model_df_all_scored <- bind_rows(results_all_new, results_all_repeat)

# ── PROBABILITY DISTRIBUTION PLOTS ──────────────────────────────────────────

plot_dist <- function(df, label) {
  df %>%
    ggplot(aes(x = prob_ensemble, fill = factor(as.integer(purchased)))) +
    geom_histogram(bins = 50, alpha = 0.6, position = "identity") +
    scale_fill_manual(values = c("0" = "#90CAF9", "1" = "#1565C0"),
                      labels = c("Not purchased", "Purchased")) +
    scale_x_continuous(labels = scales::percent) +
    labs(title = paste("Predicted purchase probability —", label),
         x = "Predicted probability", y = "Count", fill = NULL) +
    theme_minimal(base_size = 12)
}

for (.res in list(
  list(results_new,        "Never-purchased visitors (2+ games)"),
  list(results_repeat,     "Prior purchasers (2+ games)"),
  list(results_all_new,    "Never-purchased visitors (all)"),
  list(results_all_repeat, "Prior purchasers (all)")
)) {
  if (nrow(.res[[1]]) > 0 && any(!is.na(.res[[1]]$prob_ensemble))) {
    print(plot_dist(.res[[1]], .res[[2]]))
  } else {
    cat("Skipping plot for", .res[[2]], "— no scored data.\n")
  }
}

# ── TOP HIGH-INTENT NON-PURCHASERS ──────────────────────────────────────────

high_intent <- model_df_all_scored %>%
  filter(purchased == 0) %>%
  arrange(desc(prob_ensemble)) %>%
  select(segment, visitor_id, visit_id, event_name, event_date,
         prob_xgb, prob_nn, prob_ensemble,
         visitor_ever_purchased, visitor_n_purchases,
         days_before_game, dist_open_count, cheapest_price, EVENTSCORE) %>%
  head(100)

print(high_intent)

# ── PARTIAL DEPENDENCE PLOTS (game-only features) ───────────────────────────
# Calibrated using Platt scaling to map raw XGBoost probabilities to true
# conversion rates.

out_game_only_all  <- run_models(model_df_all, features_game_only, "Game-only (all visitors)", use_keras = FALSE)
xgb_model_game_all <- out_game_only_all$xgb_model

X_game_all <- model_df_all %>%
  select(all_of(features_game_only)) %>%
  mutate(across(where(is.logical), as.integer))

# Platt scaling: fit logistic regression on raw predictions → calibrated probs
train_preds_game  <- predict(xgb_model_game_all, aligned_dmatrix(X_game_all))
calib_df_game     <- tibble(pred = train_preds_game, y = model_df_all$purchased)
calib_model_game  <- glm(y ~ pred, data = calib_df_game, family = binomial)
calibrate_game    <- function(p) predict(calib_model_game, newdata = tibble(pred = p), type = "response")

pdp_manual <- function(feat, model, df, features, calibrate_fn = NULL) {
  feat_range <- seq(
    quantile(df[[feat]], 0.05, na.rm = TRUE),
    quantile(df[[feat]], 0.95, na.rm = TRUE),
    length.out = 50
  )
  probs <- map_dbl(feat_range, function(val) {
    X_temp <- df %>% mutate(!!feat := val) %>% as.matrix()
    raw    <- mean(predict(model, aligned_dmatrix(X_temp)), na.rm = TRUE)
    if (!is.null(calibrate_fn)) calibrate_fn(raw) else raw
  })
  tibble(x = feat_range, prob = probs) %>%
    ggplot(aes(x = x, y = prob)) +
    geom_line(color = "#1565C0", linewidth = 1) +
    geom_smooth(se = FALSE, color = "#B71C1C", linetype = "dashed", linewidth = 0.7) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
    labs(title = paste("Partial dependence —", feat),
         x = feat, y = "Predicted purchase probability (calibrated)") +
    theme_minimal(base_size = 12)
}

walk(features_game_only, ~print(pdp_manual(.x, xgb_model_game_all, X_game_all,
                                            features_game_only, calibrate_fn = calibrate_game)))

# ── PARTIAL DEPENDENCE PLOTS (all price variables) ──────────────────────────

X_all_prices <- model_df_all %>%
  select(all_of(features_with_all_prices)) %>%
  mutate(across(where(is.logical), as.integer))

y <- model_df_all$purchased
set.seed(42)
train_idx_p <- sample(nrow(model_df_all), 0.8 * nrow(model_df_all))
dtrain_p    <- aligned_dmatrix(X_all_prices[train_idx_p, ], label = y[train_idx_p])

xgb_model_prices <- xgb.train(
  params = list(
    objective = "binary:logistic", eval_metric = "auc", max_depth = 4,
    eta = 0.05, subsample = 0.8, colsample_bytree = 0.8,
    scale_pos_weight = sum(y[train_idx_p] == 0) / sum(y[train_idx_p] == 1)
  ),
  data = dtrain_p, nrounds = 200, verbose = 0
)

train_preds_prices <- predict(xgb_model_prices, aligned_dmatrix(X_all_prices))
calib_df_prices    <- tibble(pred = train_preds_prices, y = model_df_all$purchased)
calib_model_prices <- glm(y ~ pred, data = calib_df_prices, family = binomial)
calibrate_prices   <- function(p) predict(calib_model_prices, newdata = tibble(pred = p), type = "response")

walk(c("avg_price", "priciest_price"),
     ~print(pdp_manual(.x, xgb_model_prices, X_all_prices,
                        features_with_all_prices, calibrate_fn = calibrate_prices)))

# ── LOGISTIC REGRESSION BASELINE ────────────────────────────────────────────

glm_model <- glm(purchased ~ days_before_game + dist_open_count + cheapest_price +
                    EVENTSCORE + hour_of_day + is_weekend,
                 data = model_df_all, family = binomial)

summary(glm_model)

# ── SEGMENTED ANALYSIS BY GAME TIER ─────────────────────────────────────────

eventscore_median <- median(model_df_all$EVENTSCORE, na.rm = TRUE)

df_high_demand <- model_df_all %>% filter(EVENTSCORE >= eventscore_median)
df_low_demand  <- model_df_all %>% filter(EVENTSCORE <  eventscore_median)

cat("High demand — Rows:", nrow(df_high_demand),
    "| Purchase rate:", scales::percent(mean(df_high_demand$purchased), accuracy = 0.01), "\n")
cat("Low demand  — Rows:", nrow(df_low_demand),
    "| Purchase rate:", scales::percent(mean(df_low_demand$purchased), accuracy = 0.01), "\n")

out_high <- run_models(df_high_demand, features_game_only, "High demand games", use_keras = FALSE)
out_low  <- run_models(df_low_demand,  features_game_only, "Low demand games",  use_keras = FALSE)

xgb_model_high <- out_high$xgb_model
xgb_model_low  <- out_low$xgb_model

# KEY FINDING:
# High Demand: Cheapest price matters most, followed by days before game,
#              dist open count, hour of day. Eventscore matters least.
# Low Demand:  Hour of day matters most, followed by days before game,
#              dist open count, and Eventscore. Cheapest price matters least.

# ── TIER PDPs ────────────────────────────────────────────────────────────────

X_high <- df_high_demand %>%
  select(all_of(features_game_only)) %>%
  mutate(across(where(is.logical), as.integer))

X_low <- df_low_demand %>%
  select(all_of(features_game_only)) %>%
  mutate(across(where(is.logical), as.integer))

# Calibrate each tier model
train_preds_high <- predict(xgb_model_high, aligned_dmatrix(X_high))
calib_high <- glm(df_high_demand$purchased ~ train_preds_high, family = binomial)
calibrate_high <- function(p) predict(calib_high, newdata = tibble(train_preds_high = p), type = "response")

train_preds_low <- predict(xgb_model_low, aligned_dmatrix(X_low))
calib_low <- glm(df_low_demand$purchased ~ train_preds_low, family = binomial)
calibrate_low <- function(p) predict(calib_low, newdata = tibble(train_preds_low = p), type = "response")

# Side-by-side high vs low demand PDPs for each feature
walk(features_game_only, function(feat) {
  p_high <- pdp_manual(feat, xgb_model_high, X_high, features_game_only, calibrate_fn = calibrate_high) +
    labs(title = paste("High demand —", feat))
  p_low  <- pdp_manual(feat, xgb_model_low,  X_low,  features_game_only, calibrate_fn = calibrate_low) +
    labs(title = paste("Low demand —", feat))
  print(p_high + p_low)
})

# ── K-MEANS VISITOR CLUSTERING ──────────────────────────────────────────────

visitor_features <- model_df_all %>%
  group_by(visitor_id) %>%
  summarize(
    total_games_viewed     = n(),
    total_visits           = n_distinct(visit_id),
    ever_purchased         = max(purchased),
    total_purchases        = sum(purchased),
    avg_days_before_game   = mean(days_before_game, na.rm = TRUE),
    avg_session_size       = mean(session_games_viewed, na.rm = TRUE),
    avg_times_viewed_games = mean(n_times_viewed_game + 1, na.rm = TRUE),
    avg_eventscore         = mean(EVENTSCORE, na.rm = TRUE),
    avg_cheapest_price     = mean(cheapest_price, na.rm = TRUE),
    avg_dist_open          = mean(dist_open_count, na.rm = TRUE),
    .groups = "drop"
  )

cat("Distinct visitors:", nrow(visitor_features), "\n")

cluster_vars <- c(
  "total_games_viewed", "total_visits", "avg_days_before_game",
  "avg_session_size", "avg_times_viewed_games", "avg_eventscore",
  "avg_cheapest_price", "avg_dist_open"
)

# Replace remaining NAs with column medians, then scale.
# Drop any zero-variance columns (scale() would produce NaN).
X_cluster_raw <- visitor_features %>%
  select(all_of(cluster_vars)) %>%
  mutate(across(everything(), ~replace(.x, is.na(.x), median(.x, na.rm = TRUE))))

# Identify columns with non-zero variance
keep_cols <- sapply(X_cluster_raw, function(col) sd(col, na.rm = TRUE) > 0)
X_cluster <- X_cluster_raw %>%
  select(all_of(names(keep_cols)[keep_cols])) %>%
  scale()

# Safety: replace any remaining NaN/Inf (shouldn't happen, but belt-and-suspenders)
X_cluster[!is.finite(X_cluster)] <- 0

# Elbow plot (on a sample for speed)
set.seed(42)
sample_size <- min(nrow(X_cluster), 20000)
sample_idx  <- sample(nrow(X_cluster), sample_size)
X_sample    <- X_cluster[sample_idx, ]

wss <- map_dbl(2:10, function(k) {
  kmeans(X_sample, centers = k, nstart = 5, iter.max = 50)$tot.withinss
})

tibble(k = 2:10, wss = wss) %>%
  ggplot(aes(x = k, y = wss)) +
  geom_line(color = "#1565C0") +
  geom_point(color = "#1565C0", size = 3) +
  scale_x_continuous(breaks = 2:10) +
  labs(title = "Elbow plot — optimal number of clusters",
       x = "Number of clusters (k)", y = "Total within-cluster sum of squares") +
  theme_minimal(base_size = 12)

# Fit k=5
set.seed(42)
km_fit <- kmeans(X_cluster, centers = 5, nstart = 25, iter.max = 100)

visitor_features <- visitor_features %>%
  mutate(cluster = factor(km_fit$cluster))

cat("Cluster sizes:\n")
print(table(visitor_features$cluster))

# ── CLUSTER PROFILES ─────────────────────────────────────────────────────────

cluster_profiles <- visitor_features %>%
  group_by(cluster) %>%
  summarise(
    n                  = n(),
    pct                = scales::percent(n() / nrow(visitor_features), accuracy = 0.1),
    purchase_rate      = scales::percent(mean(ever_purchased), accuracy = 0.1),
    avg_total_games    = round(mean(total_games_viewed), 1),
    avg_visits         = round(mean(total_visits), 1),
    avg_days_out       = round(mean(avg_days_before_game), 1),
    avg_session_size   = round(mean(avg_session_size), 1),
    avg_times_viewed   = round(mean(avg_times_viewed_games), 1),
    avg_eventscore     = round(mean(avg_eventscore), 1),
    avg_cheapest_price = round(mean(avg_cheapest_price), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(purchase_rate))

print(cluster_profiles)

# ── ASSIGN LABELS BASED ON CHARACTERISTICS ───────────────────────────────────

cluster_label_df <- cluster_profiles %>%
  mutate(
    purchase_rate_n = as.numeric(sub("%", "", purchase_rate)),
    cluster_name = case_when(
      purchase_rate_n == max(purchase_rate_n) ~ "High Intent Repeat Browsers",
      avg_days_out    == min(avg_days_out)    ~ "Last Minute Browsers",
      avg_days_out    == max(avg_days_out)    ~ "Early Browsers",
      avg_eventscore  == max(avg_eventscore)  ~ "Premium Game Browsers",
      TRUE                                    ~ "Active Session Browsers"
    )
  ) %>%
  select(cluster, cluster_name)

cluster_labels <- setNames(cluster_label_df$cluster_name, as.character(cluster_label_df$cluster))

visitor_features <- visitor_features %>%
  left_join(cluster_label_df, by = "cluster")

# ── BUBBLE CHART ─────────────────────────────────────────────────────────────

cluster_summary <- visitor_features %>%
  group_by(cluster) %>%
  summarise(
    n             = n(),
    purchase_rate = mean(ever_purchased),
    avg_days_out  = mean(avg_days_before_game),
    .groups = "drop"
  ) %>%
  mutate(cluster_name = cluster_labels[as.character(cluster)])

cluster_summary %>%
  ggplot(aes(x = avg_days_out, y = purchase_rate,
             size = n, color = cluster_name, label = cluster_name)) +
  geom_point(alpha = 0.7) +
  geom_text_repel(size = 3.5, show.legend = FALSE,
                  box.padding = 1.5, point.padding = 1.0,
                  force = 10, force_pull = 0.1, max.overlaps = Inf) +
  scale_size_continuous(range = c(4, 20), guide = "none") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1),
                     limits = c(0, 0.35)) +
  scale_color_brewer(palette = "Set1", guide = "none") +
  labs(
    title    = "Visitor segments — days out vs. conversion rate",
    subtitle = "Bubble size = segment population",
    x        = "Avg days before game at browse time",
    y        = "Purchase conversion rate"
  ) +
  theme_minimal(base_size = 12)

# ── PER-CLUSTER CONVERSION MODELS ───────────────────────────────────────────

model_df_clustered <- model_df_all %>%
  left_join(visitor_features %>% select(visitor_id, cluster, cluster_name),
            by = "visitor_id")

cluster_model_results <- map(levels(visitor_features$cluster), function(cl) {
  df_cl <- model_df_clustered %>% filter(cluster == cl)
  label <- cluster_labels[cl]

  cat("\nCluster", cl, "-", label,
      "| Rows:", nrow(df_cl),
      "| Purchase rate:", scales::percent(mean(df_cl$purchased), accuracy = 0.01), "\n")

  if (sum(df_cl$purchased) < 100) {
    cat("Skipping — too few purchases\n")
    return(NULL)
  }

  run_models(df_cl, features_game_only, label, use_keras = FALSE)
})

names(cluster_model_results) <- cluster_labels

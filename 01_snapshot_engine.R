# ══════════════════════════════════════════════════════════════════════════════
# 01 — Temporal Snapshot Engine & Game Tracking Dashboards
# ══════════════════════════════════════════════════════════════════════════════
#
# PURPOSE:
#   Build point-in-time price and inventory snapshots for each website visit
#   to understand what a visitor saw when they browsed. This enables causal
#   analysis of how dynamic pricing affects purchase decisions.
#
# APPROACH:
#   1. Construct temporal INTERVALS from the change journal:
#      - Price intervals:  price_code × [start, end) → price
#      - Class intervals:  seat_location × [start, end) → class (e.g., DIST-OPEN)
#   2. Backwards-calculate historical DIST-OPEN seat counts from current
#      inventory, adjusting for sales and reclassifications after each timestamp.
#   3. Compute price snapshots: cheapest / priciest / avg among DIST-OPEN seats.
#   4. Rolling-join snapshots to every clickstream hit via data.table.
#   5. Link purchases: match Confirmation page → most recent Event Detail hit
#      in the same visit, then join ticket order details.
#   6. Generate game tracking dashboards (ggplot2 + patchwork).
#
# This is the PROTOTYPE version processing 2 focus games. See Script 02 for
# the production-scale pipeline that handles all ~80 games.
#
# INPUT:
#   data/change_journal_price.csv, data/change_journal_reclass.csv,
#   data/clickstream.csv, data/inventory.csv, data/manifest.csv,
#   data/grandstand_orders.csv, data/sales_by_date.csv
#
# OUTPUT:
#   Game tracking dashboards (printed), visit_event_summary table
#
# ══════════════════════════════════════════════════════════════════════════════

library(dplyr)
library(stringr)
library(purrr)
library(data.table)
library(ggplot2)
library(patchwork)
library(scales)
library(lubridate)

# ── LOAD DATA ────────────────────────────────────────────────────────────────

manifest <- read.csv("data/manifest.csv", stringsAsFactors = FALSE) %>%
  mutate(section_name = as.character(section_name),
         row_name     = as.character(row_name))

cj_reclass <- read.csv("data/change_journal_reclass.csv", stringsAsFactors = FALSE) %>%
  mutate(upd_datetime = as.POSIXct(upd_datetime, tz = "America/New_York"))

change_journal <- read.csv("data/change_journal_price.csv", stringsAsFactors = FALSE) %>%
  mutate(upd_datetime = as.POSIXct(upd_datetime, tz = "America/New_York"))

clickstream <- read.csv("data/clickstream.csv", stringsAsFactors = FALSE) %>%
  mutate(
    hit_timestamp = as.POSIXct(hit_timestamp, tz = "America/New_York"),
    event_date    = as.Date(event_date)
  )

inventory <- read.csv("data/inventory.csv", stringsAsFactors = FALSE) %>%
  mutate(add_datetime = as.POSIXct(add_datetime, tz = "America/New_York"),
         section_name = as.character(section_name),
         row_name     = as.character(row_name))

grandstand_orders <- read.csv("data/grandstand_orders.csv", stringsAsFactors = FALSE) %>%
  mutate(
    transaction_date = as.POSIXct(transaction_date, tz = "America/New_York"),
    event_date       = as.Date(event_date),
    section          = as.character(section),
    row              = as.character(row)
  )

sales_data <- read.csv("data/sales_by_date.csv", stringsAsFactors = FALSE) %>%
  mutate(
    event_date = as.Date(event_date),
    add_date   = as.Date(add_date)
  )

event_lookup <- read.csv("data/event_schedule.csv", stringsAsFactors = FALSE) %>%
  mutate(event_date = as.Date(event_date)) %>%
  select(event_name = event_id, event_date)

# ── HELPERS ──────────────────────────────────────────────────────────────────

INF_TIME <- as.POSIXct("2099-12-31 23:59:59", tz = "America/New_York")
events   <- c("E6BB0424", "E6BB0605")  # Two focus games for the prototype

# ── BUILD PRICE INTERVALS PER EVENT ─────────────────────────────────────────
# Each price code has a series of prices over time (Insert → Update → ...).
# We turn these into non-overlapping intervals: [start, end) → price.

build_price_intervals <- function(ev) {
  cj <- change_journal %>% filter(event_name == ev)

  # Inserts: initial price is the first pipe-delimited field in the blob
  insert_prices <- cj %>%
    filter(action_name == "Insert", nchar(price_code) == 1) %>%
    mutate(price = as.numeric(map_chr(new_value, ~ str_split(.x, "\\|")[[1]][1]))) %>%
    select(price_code, upd_datetime, price)

  # Updates: explicit old_value → new_value for the price column
  update_prices <- cj %>%
    filter(action_name == "Update", column_name == "price", nchar(price_code) == 1) %>%
    mutate(price = as.numeric(new_value)) %>%
    select(price_code, upd_datetime, price)

  # Combine and build intervals
  bind_rows(insert_prices, update_prices) %>%
    arrange(price_code, upd_datetime) %>%
    left_join(
      cj %>%
        filter(action_name == "Delete", nchar(price_code) == 1) %>%
        select(price_code, deleted_at = upd_datetime),
      by = "price_code"
    ) %>%
    group_by(price_code) %>%
    mutate(
      interval_start    = upd_datetime,
      interval_end      = lead(upd_datetime, default = INF_TIME),
      # Exclude rows in the window after a Delete (before a potential re-Insert)
      in_deleted_window = !is.na(deleted_at) &
        interval_start >= deleted_at &
        interval_start < suppressWarnings(min(upd_datetime[upd_datetime > deleted_at], na.rm = TRUE))
    ) %>%
    filter(!in_deleted_window) %>%
    ungroup() %>%
    select(price_code, price, interval_start, interval_end) %>%
    mutate(event_name = ev)
}

# ── BUILD CLASS INTERVALS PER EVENT ──────────────────────────────────────────
# The reclass journal records when seats move between classes (e.g., HOLD →
# DIST-OPEN). We reconstruct the class of each seat_location at any time.

build_class_intervals <- function(ev) {
  cj <- cj_reclass %>%
    filter(event_name == ev) %>%
    mutate(
      section_name = str_extract(seat_location, "^[^/]+"),
      row_name     = str_extract(seat_location, "(?<=/)[^/]+(?=/)")
    ) %>%
    left_join(
      manifest %>% distinct(section_name, row_name, pc_family),
      by = c("section_name", "row_name")
    )

  # Historical intervals (old_value was active from prior change to this change)
  historical <- cj %>%
    arrange(seat_location, upd_datetime) %>%
    group_by(seat_location) %>%
    mutate(
      class          = old_value,
      interval_start = lag(upd_datetime),
      interval_end   = upd_datetime
    ) %>%
    filter(!is.na(interval_start)) %>%
    ungroup()

  # Current interval (new_value from last change → infinity)
  current <- cj %>%
    arrange(seat_location, upd_datetime) %>%
    group_by(seat_location) %>%
    slice_max(upd_datetime, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(
      class          = new_value,
      interval_start = upd_datetime,
      interval_end   = INF_TIME
    ) %>%
    select(seat_location, section_name, row_name, pc_family,
           class, interval_start, interval_end)

  bind_rows(historical, current) %>%
    filter(interval_start < interval_end) %>%
    select(seat_location, section_name, row_name, pc_family,
           class, interval_start, interval_end) %>%
    arrange(seat_location, interval_start) %>%
    mutate(event_name = ev)
}

# ── HISTORICAL DIST-OPEN COUNT ──────────────────────────────────────────────
# We know the CURRENT available DIST-OPEN count. To get the count at any past
# time t, we add back: (a) seats sold after t that were DIST-OPEN at sale time,
# and (b) seats reclassed out of DIST-OPEN after t, minus those reclassed in.

build_dist_open_history <- function(ev, class_ivls) {
  # Current available DIST-OPEN count (excludes sold/comp)
  current_count <- inventory %>%
    filter(event_name == ev, !status %in% c("SOLD", "COMP"),
           class_name == "DIST-OPEN") %>%
    summarise(n = sum(num_seats)) %>%
    pull(n)

  # Sold/comp seats with their sale datetime
  sold <- inventory %>%
    filter(event_name == ev, status %in% c("SOLD", "COMP"),
           !is.na(add_datetime)) %>%
    select(section_name, row_name, seat_num, num_seats, add_datetime)

  # Look up each sold seat's class at time of sale via data.table non-equi join
  dt_class <- as.data.table(class_ivls)
  dt_sold  <- as.data.table(sold)
  setkey(dt_sold, section_name, row_name, add_datetime)

  sold_with_class <- dt_class[
    dt_sold,
    .(section_name, row_name, seat_num, num_seats, add_datetime, class),
    on = .(section_name = section_name,
           row_name     = row_name,
           interval_start <= add_datetime,
           interval_end   >  add_datetime),
    nomatch = NA
  ] %>% as_tibble()

  # Reclass changes for adjustment
  reclass_changes <- cj_reclass %>%
    filter(event_name == ev) %>%
    mutate(
      section_name = str_extract(seat_location, "^[^/]+"),
      row_name     = str_extract(seat_location, "(?<=/)[^/]+(?=/)")
    ) %>%
    select(section_name, row_name, old_value, new_value, upd_datetime)

  # All distinct change times to compute snapshots at
  change_times <- sort(unique(c(
    reclass_changes$upd_datetime,
    sold_with_class$add_datetime
  )))

  if (length(change_times) == 0) {
    return(data.table(hit_timestamp = as.POSIXct(character(0)),
                      dist_open_count = integer(0)))
  }

  # For each change time, compute DIST-OPEN count via backwards adjustment
  dist_open_history <- data.table(
    hit_timestamp   = change_times,
    dist_open_count = sapply(change_times, function(t) {
      # Reclass adjustment: seats moved in/out of DIST-OPEN after time t
      reclass_adj <- reclass_changes %>%
        filter(upd_datetime > t) %>%
        summarise(
          into  = sum(new_value == "DIST-OPEN"),
          outof = sum(old_value == "DIST-OPEN")
        )

      # Sales adjustment: DIST-OPEN seats sold after time t
      sales_adj <- sold_with_class %>%
        filter(add_datetime > t, class == "DIST-OPEN") %>%
        summarise(n = sum(num_seats)) %>%
        pull(n)

      current_count - reclass_adj$into + reclass_adj$outof + sales_adj
    })
  )

  dist_open_history
}

# ── COMPUTE PRICE SNAPSHOTS PER EVENT ────────────────────────────────────────
# At each change point, find the cheapest/priciest/avg price among price codes
# that map to DIST-OPEN seats (via pc_family).

compute_snapshots <- function(ev, dt_price, dt_class, dist_open_hist) {
  price_change_times <- sort(unique(c(dt_price$interval_start, dt_class$interval_start)))

  price_snapshots <- data.table(
    hit_timestamp = price_change_times,
    cheapest_price = sapply(price_change_times, function(t) {
      fams <- dt_class[interval_start <= t & interval_end > t & class == "DIST-OPEN" & !is.na(pc_family), unique(pc_family)]
      p    <- dt_price[interval_start <= t & interval_end > t & price_code %in% fams]
      if (nrow(p)) p[which.min(price), price] else NA_real_
    }),
    cheapest_pc_family = sapply(price_change_times, function(t) {
      fams <- dt_class[interval_start <= t & interval_end > t & class == "DIST-OPEN" & !is.na(pc_family), unique(pc_family)]
      p    <- dt_price[interval_start <= t & interval_end > t & price_code %in% fams]
      if (nrow(p)) p[which.min(price), price_code] else NA_character_
    }),
    priciest_price = sapply(price_change_times, function(t) {
      fams <- dt_class[interval_start <= t & interval_end > t & class == "DIST-OPEN" & !is.na(pc_family), unique(pc_family)]
      p    <- dt_price[interval_start <= t & interval_end > t & price_code %in% fams]
      if (nrow(p)) p[which.max(price), price] else NA_real_
    }),
    priciest_pc_family = sapply(price_change_times, function(t) {
      fams <- dt_class[interval_start <= t & interval_end > t & class == "DIST-OPEN" & !is.na(pc_family), unique(pc_family)]
      p    <- dt_price[interval_start <= t & interval_end > t & price_code %in% fams]
      if (nrow(p)) p[which.max(price), price_code] else NA_character_
    }),
    avg_price = sapply(price_change_times, function(t) {
      fams <- dt_class[interval_start <= t & interval_end > t & class == "DIST-OPEN" & !is.na(pc_family),
                       .(seat_count = .N), by = pc_family]
      p    <- dt_price[interval_start <= t & interval_end > t & price_code %in% fams$pc_family]
      if (nrow(p) == 0) return(NA_real_)
      m <- merge(p, fams, by.x = "price_code", by.y = "pc_family")
      sum(m$price * m$seat_count) / sum(m$seat_count)
    })
  )

  # Rolling join snapshots to distinct hit timestamps for this event
  dt_hits <- data.table(hit_timestamp = sort(unique(
    clickstream$hit_timestamp[clickstream$event_name == ev]
  )))
  setkey(dt_hits,          hit_timestamp)
  setkey(dist_open_hist,   hit_timestamp)
  setkey(price_snapshots,  hit_timestamp)

  dist_joined  <- dist_open_hist[dt_hits,  roll = TRUE]
  price_joined <- price_snapshots[dt_hits, roll = TRUE]

  dist_joined[price_joined, on = "hit_timestamp"] %>%
    as_tibble() %>%
    mutate(event_name = ev)
}

# ── RUN FOR EACH EVENT AND JOIN TO CLICKSTREAM ───────────────────────────────

cat("Building snapshots for focus events...\n")

snapshots_all <- map_dfr(events, function(ev) {
  cat("  Processing", ev, "...\n")
  dt_price       <- as.data.table(build_price_intervals(ev))
  class_ivls     <- build_class_intervals(ev)
  dt_class       <- as.data.table(class_ivls)
  dist_open_hist <- build_dist_open_history(ev, class_ivls)
  compute_snapshots(ev, dt_price, dt_class, dist_open_hist)
})

result <- clickstream %>%
  left_join(snapshots_all, by = c("event_name", "hit_timestamp"))

cat("Rows in result:", nrow(result), "\n")

# ── GAME TRACKING DASHBOARD ─────────────────────────────────────────────────
# Three-panel dashboard per game:
#   1. Conversion rate vs DIST-OPEN availability (dual axis)
#   2. Weekly conversion rate bars
#   3. Price range over time (cheapest / priciest / weighted average)

run_game_tracking <- function(target_event, sales_df) {
  target_event_date <- event_lookup$event_date[event_lookup$event_name == target_event]

  daily_hits <- result %>%
    filter(event_name == target_event) %>%
    mutate(date = as.Date(hit_timestamp)) %>%
    group_by(date) %>%
    summarise(
      total_hits    = n(),
      unique_visits = n_distinct(visit_id),
      .groups = "drop"
    )

  daily_snapshot <- result %>%
    filter(event_name == target_event) %>%
    mutate(date = as.Date(hit_timestamp)) %>%
    group_by(date) %>%
    slice_min(hit_timestamp, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(date, dist_open_count, cheapest_price, cheapest_pc_family,
           priciest_price, priciest_pc_family, avg_price)

  tracking <- daily_hits %>%
    left_join(daily_snapshot, by = "date") %>%
    left_join(sales_df %>% select(date = add_date, transactions, num_seats,
                                  unique_buyers, total_revenue),
              by = "date") %>%
    mutate(
      transactions    = coalesce(transactions, 0L),
      num_seats       = coalesce(num_seats, 0),
      unique_buyers   = coalesce(unique_buyers, 0L),
      total_revenue   = coalesce(total_revenue, 0),
      conversion_rate = transactions / total_hits
    ) %>%
    arrange(date) %>%
    filter(date <= target_event_date, date >= as.Date("2025-11-12"))

  if (nrow(tracking) == 0) {
    cat("No tracking data for", target_event, "\n")
    return(invisible(NULL))
  }

  conv_max   <- max(tracking$conversion_rate, na.rm = TRUE)
  dist_max   <- max(tracking$dist_open_count, na.rm = TRUE)
  dist_scale <- if (dist_max > 0) conv_max / dist_max else 1

  # Panel 1: Conversion rate (line) vs DIST-OPEN count (bars)
  p1 <- ggplot(tracking, aes(x = date)) +
    geom_col(aes(y = dist_open_count * dist_scale), fill = "#90CAF9", alpha = 0.5) +
    geom_line(aes(y = conversion_rate), color = "#1565C0", linewidth = 1) +
    geom_point(aes(y = conversion_rate), color = "#1565C0", size = 2) +
    scale_y_continuous(
      name = "Conversion Rate (Transactions / Hit)",
      labels = percent_format(accuracy = 0.01),
      sec.axis = sec_axis(~ . / dist_scale, name = "DIST-OPEN Seat Count", labels = comma)
    ) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    labs(title    = paste("Game Tracking —", target_event,
                          format(target_event_date, "(%B %d, %Y)")),
         subtitle = "Blue line = conversion rate  |  Blue bars = DIST-OPEN availability",
         x = NULL) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title  = element_text(face = "bold"))

  # Panel 2: Weekly conversion rate
  weekly <- tracking %>%
    mutate(week = floor_date(date, "week", week_start = 1)) %>%
    group_by(week) %>%
    summarise(total_hits = sum(total_hits), transactions = sum(transactions), .groups = "drop") %>%
    mutate(conversion_rate = transactions / total_hits)

  p3 <- ggplot(weekly, aes(x = week, y = conversion_rate)) +
    geom_col(fill = "#1565C0", alpha = 0.7) +
    geom_text(aes(label = percent(conversion_rate, accuracy = 0.01)),
              vjust = -0.5, size = 3, color = "#1565C0") +
    scale_y_continuous(name = "Conversion Rate (Transactions / Hit)",
                       labels = percent_format(accuracy = 0.01)) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    labs(x = NULL, subtitle = "Weekly conversion rate") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Panel 3: Price range over time
  p2 <- ggplot(tracking, aes(x = date)) +
    geom_ribbon(aes(ymin = cheapest_price, ymax = priciest_price),
                fill = "#A5D6A7", alpha = 0.4, na.rm = TRUE) +
    geom_line(aes(y = cheapest_price, color = "Cheapest"), linewidth = 1, na.rm = TRUE) +
    geom_line(aes(y = priciest_price, color = "Priciest"), linewidth = 1, na.rm = TRUE) +
    geom_line(aes(y = avg_price, color = "Avg (seat-weighted)"),
              linewidth = 1, linetype = "dashed", na.rm = TRUE) +
    scale_color_manual(values = c(
      "Cheapest"            = "#2E7D32",
      "Priciest"            = "#B71C1C",
      "Avg (seat-weighted)" = "#6A1B9A"
    )) +
    scale_y_continuous(name = "Price ($)", labels = dollar_format()) +
    scale_x_date(date_breaks = "1 week", date_labels = "%b %d") +
    labs(x = "Date", color = NULL,
         subtitle = "Price range with seat-weighted average (dashed)") +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")

  print(p1 / p3 / p2 + plot_layout(heights = c(2, 1, 1)))
  invisible(tracking)
}

# ── RUN GAME TRACKING ────────────────────────────────────────────────────────

sales_0424 <- sales_data %>% filter(event_name == "E6BB0424")
sales_0605 <- sales_data %>% filter(event_name == "E6BB0605")

tracking_0424 <- run_game_tracking("E6BB0424", sales_0424)
tracking_0605 <- run_game_tracking("E6BB0605", sales_0605)

# ── EXACT SNAPSHOT LOOKUP ────────────────────────────────────────────────────
# Query the snapshot for any event at any specific datetime.

get_snapshot <- function(ev, datetime) {
  dt <- as.POSIXct(datetime, tz = "America/New_York")

  snapshots_all %>%
    filter(event_name == ev, hit_timestamp <= dt) %>%
    slice_max(hit_timestamp, n = 1, with_ties = FALSE) %>%
    select(event_name, hit_timestamp, dist_open_count,
           cheapest_price, cheapest_pc_family,
           priciest_price, priciest_pc_family, avg_price)
}

get_snapshot("E6BB0605", "2026-05-26 11:00:00")

# ── LINK PURCHASES TO EVENT DETAIL HITS ──────────────────────────────────────
# Strategy: find Confirmation pages with purchase IDs, then trace back to the
# most recent Event Detail hit in the same visit (the page where the visitor
# decided to buy).

# Event Detail hits with game date extracted from product_list
event_detail_hits <- clickstream %>%
  filter(pagename == "Ticketmaster: Event Detail",
         str_detect(product_list, "\\d{1,2}/\\d{1,2}/\\d{4}")) %>%
  mutate(
    event_date = as.Date(
      str_extract(product_list, "\\d{1,2}/\\d{1,2}/\\d{4}"),
      format = "%m/%d/%Y"
    )
  )

# Confirmation hits with purchase IDs
confirmations <- clickstream %>%
  filter(
    str_detect(pagename, "Confirmation"),
    !is.na(purchase_id),
    str_detect(product_list, " vs\\.")
  ) %>%
  select(visit_id, purchase_id, hit_timestamp)

# For each confirmation, find the most recent Event Detail hit before it
result_linked <- event_detail_hits %>%
  select(-purchase_id) %>%
  inner_join(
    confirmations %>% select(visit_id, purchase_id, confirmation_time = hit_timestamp),
    by = "visit_id",
    relationship = "many-to-many"
  ) %>%
  filter(hit_timestamp <= confirmation_time) %>%
  group_by(visit_id, purchase_id, confirmation_time) %>%
  slice_max(hit_timestamp, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(hit_id, visit_id, purchase_id, event_date)

# ── BUILD visits_with_date_and_purchase ──────────────────────────────────────
# All hits for visits that have a game date in product_list

visits_with_date <- clickstream %>%
  filter(str_detect(product_list, "\\d{1,2}/\\d{1,2}/\\d{4}")) %>%
  distinct(visit_id)

purchase_event_date <- result_linked %>%
  select(visit_id, purchase_id, event_date)

visits_with_date_and_purchase <- clickstream %>%
  filter(visit_id %in% visits_with_date$visit_id) %>%
  left_join(
    result_linked %>% select(hit_id, purchase_id_linked = purchase_id, event_date_linked = event_date),
    by = "hit_id"
  ) %>%
  left_join(
    purchase_event_date %>% rename(event_date_from_purchase = event_date),
    by = c("visit_id", "purchase_id")
  ) %>%
  mutate(
    event_date = coalesce(event_date, event_date_linked, event_date_from_purchase)
  ) %>%
  select(-event_date_linked, -event_date_from_purchase) %>%
  arrange(visit_id, hit_timestamp) %>%
  mutate(hit_timestamp = as.character(hit_timestamp))

cat("Total hits in qualifying visits:", nrow(visits_with_date_and_purchase), "\n")
cat("Hits with a purchase linked:", sum(!is.na(visits_with_date_and_purchase$purchase_id_linked)), "\n")

# ── JOIN WITH TICKET ORDER DATA ──────────────────────────────────────────────
# Extract order_key from the purchase_id and join to grandstand_orders for
# seat location, price, and section details.

visits_with_date_and_purchase <- visits_with_date_and_purchase %>%
  mutate(
    order_key = case_when(
      str_detect(purchase_id, "/") ~ str_sub(purchase_id, 8, 15),
      !is.na(purchase_id)          ~ str_extract(purchase_id, "(?<=ATL:TM_)\\d{8}")
    )
  )

visits_final <- visits_with_date_and_purchase %>%
  left_join(grandstand_orders %>% select(-event_date), by = "order_key")

cat("Rows in visits final:", nrow(visits_final), "\n")

# ── ENRICH WITH SNAPSHOT DATA ────────────────────────────────────────────────

purchase_info <- visits_final %>%
  filter(!is.na(purchase_id), !is.na(pc_family)) %>%
  select(purchase_id, pc_family, price_location) %>%
  distinct()

purchase_lookup <- result_linked %>%
  left_join(purchase_info, by = "purchase_id") %>%
  select(hit_id, purchase_id_linked = purchase_id,
         event_date, pc_family, price_location)

result_w_pc_family <- result %>%
  left_join(purchase_lookup, by = "hit_id") %>%
  mutate(event_date = coalesce(event_date.x, event_date.y)) %>%
  select(-event_date.x, -event_date.y)

qualifying_visits <- result %>%
  distinct(visit_id)

# Confirmation row lookup
confirmation_lookup <- result_linked %>%
  left_join(purchase_info, by = "purchase_id") %>%
  left_join(
    result_w_pc_family %>% select(purchase_id_linked, dist_open_count, cheapest_price,
                                   cheapest_pc_family, priciest_price, priciest_pc_family, avg_price),
    by = c("purchase_id" = "purchase_id_linked")
  ) %>%
  select(purchase_id_linked = purchase_id, event_date, pc_family, price_location,
         dist_open_count, cheapest_price, cheapest_pc_family, priciest_price,
         priciest_pc_family, avg_price) %>%
  distinct()

# ── ASSEMBLE FINAL RESULT ────────────────────────────────────────────────────

result_game <- clickstream %>%
  filter(visit_id %in% qualifying_visits$visit_id) %>%
  left_join(
    result_w_pc_family %>% select(hit_id, event_name_snapshot = event_name,
                                  event_date_snapshot = event_date, dist_open_count, cheapest_price,
                                  cheapest_pc_family, priciest_price, priciest_pc_family, avg_price,
                                  purchase_id_linked, pc_family, price_location),
    by = "hit_id"
  ) %>%
  left_join(
    confirmation_lookup,
    by = c("purchase_id" = "purchase_id_linked"),
    suffix = c("", "_from_purchase")
  ) %>%
  mutate(
    event_date               = coalesce(event_date, event_date_snapshot, event_date_from_purchase),
    pc_family                = coalesce(pc_family,                pc_family_from_purchase),
    price_location           = coalesce(price_location,           price_location_from_purchase),
    dist_open_count          = coalesce(dist_open_count,          dist_open_count_from_purchase),
    cheapest_price           = coalesce(cheapest_price,           cheapest_price_from_purchase),
    cheapest_pc_family       = coalesce(cheapest_pc_family,       cheapest_pc_family_from_purchase),
    priciest_price           = coalesce(priciest_price,           priciest_price_from_purchase),
    priciest_pc_family       = coalesce(priciest_pc_family,       priciest_pc_family_from_purchase),
    avg_price                = coalesce(avg_price,                avg_price_from_purchase)
  ) %>%
  select(-ends_with("_from_purchase"), -event_date_snapshot) %>%
  mutate(event_name = coalesce(event_name, event_name_snapshot)) %>%
  select(-event_name_snapshot) %>%
  # Grandstand fallback for unmatched confirmations
  mutate(
    order_key = case_when(
      str_detect(purchase_id, "/") ~ str_sub(purchase_id, 8, 15),
      !is.na(purchase_id)          ~ str_extract(purchase_id, "(?<=ATL:TM_)\\d{8}")
    )
  ) %>%
  left_join(
    grandstand_orders %>% select(order_key,
                                 event_date_gs    = event_date,
                                 pc_family_gs     = pc_family,
                                 price_loc_gs     = price_location,
                                 num_seats_gs     = num_seats,
                                 ticket_price_gs  = ticket_price,
                                 total_price_gs   = total_price,
                                 section_gs       = section,
                                 row_gs           = row),
    by = "order_key",
    relationship = "many-to-many"
  ) %>%
  group_by(hit_id) %>%
  arrange(is.na(pc_family_gs)) %>%
  slice(1) %>%
  ungroup() %>%
  mutate(
    event_date     = coalesce(event_date, event_date_gs),
    pc_family      = coalesce(pc_family, pc_family_gs),
    price_location = coalesce(price_location, price_loc_gs),
    num_seats      = num_seats_gs,
    ticket_price   = ticket_price_gs,
    total_price    = total_price_gs,
    section        = section_gs,
    row            = row_gs
  ) %>%
  left_join(event_lookup, by = "event_date", suffix = c("", "_lookup")) %>%
  mutate(event_name = coalesce(event_name, event_name_lookup)) %>%
  select(-ends_with("_gs"), -event_name_lookup, -order_key) %>%
  select(hit_id, hit_timestamp, visitor_id, visit_id, pagename, product_list, purchase_id,
         purchase_id_linked, event_name, event_date, pc_family, price_location,
         section, row, num_seats, ticket_price, total_price,
         dist_open_count, cheapest_price, cheapest_pc_family,
         priciest_price, priciest_pc_family, avg_price) %>%
  arrange(visit_id, hit_timestamp)

# ── VISIT-EVENT SUMMARY ─────────────────────────────────────────────────────
# Collapse to one row per visit × event — the unit of analysis for modeling.

visit_event_summary <- result_game %>%
  filter(!is.na(event_name)) %>%
  group_by(visit_id, event_name, event_date) %>%
  summarise(
    first_hit_timestamp = min(hit_timestamp),
    purchase_id         = if (all(is.na(purchase_id))) NA_character_ else first(na.omit(purchase_id)),
    section             = if (all(is.na(section))) NA_character_ else first(na.omit(section)),
    row                 = if (all(is.na(row))) NA_character_ else first(na.omit(row)),
    price_location      = if (all(is.na(price_location))) NA_character_ else first(na.omit(price_location)),
    num_seats           = if (all(is.na(num_seats))) NA_integer_ else first(na.omit(num_seats)),
    ticket_price        = if (all(is.na(ticket_price))) NA_real_ else first(na.omit(ticket_price)),
    total_price         = if (all(is.na(total_price))) NA_real_ else first(na.omit(total_price)),
    dist_open_count     = if (all(is.na(dist_open_count))) NA_integer_ else first(na.omit(dist_open_count)),
    cheapest_price      = if (all(is.na(cheapest_price))) NA_real_ else first(na.omit(cheapest_price)),
    cheapest_pc_family  = if (all(is.na(cheapest_pc_family))) NA_character_ else first(na.omit(cheapest_pc_family)),
    priciest_price      = if (all(is.na(priciest_price))) NA_real_ else first(na.omit(priciest_price)),
    priciest_pc_family  = if (all(is.na(priciest_pc_family))) NA_character_ else first(na.omit(priciest_pc_family)),
    .groups = "drop"
  ) %>%
  mutate(purchased = !is.na(purchase_id)) %>%
  select(visit_id, event_name, event_date, first_hit_timestamp, purchase_id, purchased,
         section, row, price_location, num_seats, ticket_price, total_price,
         cheapest_price, cheapest_pc_family, priciest_price, priciest_pc_family) %>%
  arrange(visit_id, first_hit_timestamp)

cat("\nVisit-event summary:", nrow(visit_event_summary), "rows\n")
cat("Purchase rate:", scales::percent(mean(visit_event_summary$purchased), accuracy = 0.01), "\n")

# ── EXPLORE: VISITORS WHO BOUGHT SOME GAMES BUT NOT OTHERS ──────────────────

mixed_visitors <- visit_event_summary %>%
  group_by(visit_id) %>%
  filter(any(purchased), any(!purchased)) %>%
  ungroup() %>%
  arrange(visit_id, event_date)

cat("Visits with mixed purchase behavior:", n_distinct(mixed_visitors$visit_id), "\n")

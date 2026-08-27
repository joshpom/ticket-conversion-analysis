# ══════════════════════════════════════════════════════════════════════════════
# Generate Synthetic Data for Dynamic Pricing Portfolio Project
# ══════════════════════════════════════════════════════════════════════════════
#
# This script generates all CSV datasets needed by the four analysis scripts.
# The data structure mirrors the real Archtics/Ticketmaster/Wheelhouse data
# sources used in production, but all values are entirely synthetic.
#
# Team:   Atlanta Braves
# Venue:  Truist Park
# Season: 2026
#
# Run this script once from the portfolio/ directory:
#   source("data/generate_synthetic_data.R")
#
# ══════════════════════════════════════════════════════════════════════════════

library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

set.seed(2026)

# Generate random numeric strings of length n (avoids integer overflow from sample(1e18))
rand_id <- function(len = 19, n = 1) {
  vapply(seq_len(n), function(i) {
    paste0(sample(0:9, len, replace = TRUE), collapse = "")
  }, character(1))
}

out_dir <- "data"
dir.create(out_dir, showWarnings = FALSE)

cat("Generating synthetic data...\n")

# ── CONFIGURATION ────────────────────────────────────────────────────────────

opponents <- c(
  "New York Mets", "Philadelphia Phillies", "Miami Marlins",
  "Washington Nationals", "Los Angeles Dodgers", "San Diego Padres",
  "San Francisco Giants", "Chicago Cubs", "St. Louis Cardinals",
  "Milwaukee Brewers", "Cincinnati Reds", "Pittsburgh Pirates",
  "Colorado Rockies", "Arizona Diamondbacks", "Toronto Blue Jays"
)

# Price structure: letter → (base_price, DA Price Location)
price_tiers <- tibble(
  pc_family = LETTERS[1:16],
  base_price = c(250, 220, 195, 170,    # A-D: 100-level premium
                 145, 125, 110, 95,      # E-H: 200-level club
                 75, 65, 55, 45,         # I-L: 300-level upper
                 35, 28, 22, 18),        # M-P: 400-level value
  price_location = c(
    "Dugout Infield", "Lower Infield", "Lower Baseline", "Lower Outfield",
    "Infiniti Club Infield", "Infiniti Club Baseline", "Infiniti Club Outfield", "Club Terrace",
    "Vista Infield", "Vista Baseline", "Vista Outfield", "Vista Terrace",
    "SunTrust Deck Infield", "SunTrust Deck Outfield", "SunTrust Deck Terrace", "Standing Room"
  )
)

# ── 1. MANIFEST (Truist Park seating map) ───────────────────────────────────

cat("  manifest.csv\n")

manifest_rows <- list()
idx <- 1

# 100-level: sections 101-120, rows 1-8
for (sec in 101:120) {
  tier_idx <- ((sec - 101) %/% 5) + 1
  for (rw in 1:8) {
    manifest_rows[[idx]] <- list(
      section_name = as.character(sec),
      row_name     = as.character(rw),
      pc_family    = price_tiers$pc_family[tier_idx],
      price_location = price_tiers$price_location[tier_idx]
    )
    idx <- idx + 1
  }
}

# 200-level: sections 201-220, rows 1-10
for (sec in 201:220) {
  tier_idx <- ((sec - 201) %/% 5) + 5
  for (rw in 1:10) {
    manifest_rows[[idx]] <- list(
      section_name = as.character(sec),
      row_name     = as.character(rw),
      pc_family    = price_tiers$pc_family[tier_idx],
      price_location = price_tiers$price_location[tier_idx]
    )
    idx <- idx + 1
  }
}

# 300-level: sections 301-320, rows 1-12
for (sec in 301:320) {
  tier_idx <- ((sec - 301) %/% 5) + 9
  for (rw in 1:12) {
    manifest_rows[[idx]] <- list(
      section_name = as.character(sec),
      row_name     = as.character(rw),
      pc_family    = price_tiers$pc_family[tier_idx],
      price_location = price_tiers$price_location[tier_idx]
    )
    idx <- idx + 1
  }
}

# 400-level: sections 401-410, rows 1-15
for (sec in 401:410) {
  tier_idx <- min(((sec - 401) %/% 3) + 13, 16)
  for (rw in 1:15) {
    manifest_rows[[idx]] <- list(
      section_name = as.character(sec),
      row_name     = as.character(rw),
      pc_family    = price_tiers$pc_family[tier_idx],
      price_location = price_tiers$price_location[tier_idx]
    )
    idx <- idx + 1
  }
}

manifest <- bind_rows(manifest_rows)
write.csv(manifest, file.path(out_dir, "manifest.csv"), row.names = FALSE)

# ── 2. EVENT SCHEDULE ────────────────────────────────────────────────────────

cat("  event_schedule.csv\n")

# ~80 home game dates
all_dates <- seq(as.Date("2026-03-27"), as.Date("2026-09-27"), by = "day")
game_dates <- all_dates[
  runif(length(all_dates)) < 0.55 | wday(all_dates) %in% c(1, 6, 7)
]
game_dates <- sort(unique(game_dates))
if (length(game_dates) > 81) game_dates <- game_dates[1:81]

# Event ID format: E6BB + MMDD (Archtics convention)
event_ids <- paste0("E6BB", format(game_dates, "%m%d"))
# Handle any duplicates (doubleheaders)
dupes <- duplicated(event_ids)
event_ids[dupes] <- paste0(event_ids[dupes], "b")

event_schedule <- tibble(
  event_id   = event_ids,
  event_date = game_dates,
  event_time = ifelse(
    wday(game_dates) %in% c(1, 7) & runif(length(game_dates)) < 0.4,
    "13:15:00", "19:15:00"
  ),
  opponent   = sample(opponents, length(game_dates), replace = TRUE),
  season_id  = 569L
)

write.csv(event_schedule, file.path(out_dir, "event_schedule.csv"), row.names = FALSE)

# ── 3. EVENT SCORES ──────────────────────────────────────────────────────────

cat("  event_scores.csv\n")

# Premium opponents (Dodgers, Mets, Phillies) get higher scores
opponent_premium <- setNames(
  c(1.3, 1.25, 1.1, 0.85, 1.4, 1.2, 1.1, 1.15, 1.1,
    1.05, 0.9, 0.85, 0.8, 1.0, 1.1),
  opponents
)

event_scores <- event_schedule %>%
  mutate(
    base_score    = 120 + rnorm(n(), 0, 30),
    weekend_bonus = ifelse(wday(event_date) %in% c(1, 6, 7), 30, 0),
    opp_mult      = opponent_premium[opponent],
    EVENTSCORE    = round(pmax(80, pmin(250, (base_score + weekend_bonus) * opp_mult))),
    ARPS          = round(30 + (EVENTSCORE - 80) * 0.5 + rnorm(n(), 0, 8), 4),
    pct_no_show   = round(pmax(0.08, pmin(0.35, 0.22 - (EVENTSCORE - 150) * 0.001 + rnorm(n(), 0, 0.04))), 4),
    pct_sold      = round(pmax(0.45, pmin(0.98, 0.65 + (EVENTSCORE - 120) * 0.002 + rnorm(n(), 0, 0.08))), 4),
    DATETIME      = paste(event_date, event_time)
  ) %>%
  select(EVENT = opponent, DATETIME, EVENTSCORE, ARPS, pct_no_show, pct_sold)

write.csv(event_scores, file.path(out_dir, "event_scores.csv"), row.names = FALSE)

# Attach scores back for later use
ev_score_lookup <- event_scores %>%
  mutate(event_date = as.Date(str_extract(DATETIME, "^\\d{4}-\\d{2}-\\d{2}"))) %>%
  select(event_date, EVENTSCORE)
event_schedule <- event_schedule %>%
  left_join(ev_score_lookup, by = "event_date")

# ── 4. CHANGE JOURNAL — PRICE CODES ─────────────────────────────────────────

cat("  change_journal_price.csv\n")

cj_price_rows <- list()
row_idx <- 1

for (i in 1:nrow(event_schedule)) {
  ev <- event_schedule$event_id[i]
  ev_date <- event_schedule$event_date[i]

  n_codes <- sample(12:16, 1)
  codes <- LETTERS[1:n_codes]
  insert_date_base <- as.POSIXct("2025-11-01 10:00:00", tz = "America/New_York")

  for (pc in codes) {
    bp <- price_tiers$base_price[price_tiers$pc_family == pc]
    if (length(bp) == 0) bp <- 50
    init_price <- round(bp * runif(1, 0.85, 1.15), 2)
    insert_time <- insert_date_base + runif(1, 0, 45 * 86400)

    blob_fields <- paste(
      init_price, init_price, "", "I", "Standard Pricing",
      round(init_price - 10, 2), "0.00", "0.00", "10.00", "0.00",
      sep = "|"
    )

    cj_price_rows[[row_idx]] <- list(
      data_category = "Price Code", action = "I", action_name = "Insert",
      column_name = NA_character_, event_name = ev, price_code = pc,
      old_value = NA_character_, new_value = blob_fields,
      upd_user = paste0("BRA", sprintf("%03d", sample(500:510, 1))),
      upd_datetime = format(insert_time, "%Y-%m-%d %H:%M:%OS3")
    )
    row_idx <- row_idx + 1

    # 2-5 price updates
    n_updates <- sample(2:5, 1)
    current_price <- init_price
    update_start <- as.POSIXct("2026-01-15 09:00:00", tz = "America/New_York")
    update_end   <- as.POSIXct(paste(ev_date, "00:00:00"), tz = "America/New_York")

    if (update_end > update_start) {
      update_times <- sort(update_start + runif(n_updates, 0, as.numeric(update_end - update_start, units = "secs")))
      for (ut in update_times) {
        old_price <- current_price
        delta <- sample(c(-15, -10, -5, 5, 10, 15, 20, 25, 30), 1)
        current_price <- max(10, round(old_price + delta, 2))

        cj_price_rows[[row_idx]] <- list(
          data_category = "Price Code", action = "U", action_name = "Update",
          column_name = "price", event_name = ev, price_code = pc,
          old_value = as.character(old_price), new_value = as.character(current_price),
          upd_user = paste0("BRA", sprintf("%03d", sample(500:510, 1))),
          upd_datetime = format(as.POSIXct(ut, origin = "1970-01-01", tz = "America/New_York"),
                                "%Y-%m-%d %H:%M:%OS3")
        )
        row_idx <- row_idx + 1
      }
    }
  }

  # Occasional Deletes (~10% of events)
  if (runif(1) < 0.10) {
    del_code <- sample(codes, 1)
    del_time <- as.POSIXct(paste(ev_date - sample(5:30, 1), "14:00:00"), tz = "America/New_York")
    cj_price_rows[[row_idx]] <- list(
      data_category = "Price Code", action = "D", action_name = "Delete",
      column_name = NA_character_, event_name = ev, price_code = del_code,
      old_value = NA_character_, new_value = NA_character_,
      upd_user = paste0("BRA", sprintf("%03d", sample(500:510, 1))),
      upd_datetime = format(del_time, "%Y-%m-%d %H:%M:%OS3")
    )
    row_idx <- row_idx + 1
  }
}

cj_price <- bind_rows(cj_price_rows)
write.csv(cj_price, file.path(out_dir, "change_journal_price.csv"), row.names = FALSE)

# ── 5. CHANGE JOURNAL — RECLASSIFICATIONS ───────────────────────────────────

cat("  change_journal_reclass.csv\n")

reclass_transitions <- list(
  c("HOLD", "DIST-OPEN"), c("DIST-OPEN", "HOLD"),
  c("SEASON", "DIST-OPEN"), c("HOLD", "COMP"), c("GROUP", "DIST-OPEN")
)

cj_reclass_rows <- list()
row_idx <- 1
focus_events <- c("E6BB0424", "E6BB0605")
all_event_ids <- event_schedule$event_id

for (ev in all_event_ids) {
  ev_date <- event_schedule$event_date[event_schedule$event_id == ev]
  n_reclass <- if (ev %in% focus_events) sample(80:120, 1) else sample(5:30, 1)

  for (j in 1:n_reclass) {
    sec <- sample(c(101:120, 201:220, 301:320, 401:410), 1)
    rw <- sample(1:10, 1)
    seat_start <- sample(1:20, 1)
    seat_end <- seat_start + sample(0:8, 1)
    seat_loc <- paste0(sec, "/", rw, "/", seat_start, "-", seat_end)

    trans <- reclass_transitions[[sample(length(reclass_transitions), 1)]]
    reclass_time <- as.POSIXct("2025-11-10 09:00:00", tz = "America/New_York") +
      runif(1, 0, as.numeric(as.POSIXct(paste(ev_date, "00:00:00"), tz = "America/New_York") -
                             as.POSIXct("2025-11-10 09:00:00", tz = "America/New_York"), units = "secs"))

    cj_reclass_rows[[row_idx]] <- list(
      data_category = "Reclass", action = "U", action_name = "Update",
      column_name = "Hold Class", event_name = ev, seat_location = seat_loc,
      old_value = trans[1], new_value = trans[2],
      upd_user = paste0("BRA", sprintf("%03d", sample(500:510, 1))),
      upd_datetime = format(reclass_time, "%Y-%m-%d %H:%M:%OS3")
    )
    row_idx <- row_idx + 1
  }
}

cj_reclass <- bind_rows(cj_reclass_rows)
write.csv(cj_reclass, file.path(out_dir, "change_journal_reclass.csv"), row.names = FALSE)

# ── 6. INVENTORY ─────────────────────────────────────────────────────────────

cat("  inventory.csv\n")

generate_event_inventory <- function(ev, ev_date, n_seats) {
  rows <- vector("list", n_seats)
  sections <- sample(c(101:120, 201:220, 301:320, 401:410), min(n_seats, 60), replace = TRUE)

  for (s in 1:n_seats) {
    sec <- sections[((s - 1) %% length(sections)) + 1]
    rw <- sample(1:10, 1)
    seat <- sample(1:24, 1)
    class_roll <- runif(1)
    cls <- if (class_roll < 0.55) "DIST-OPEN" else if (class_roll < 0.75) "SEASON"
           else if (class_roll < 0.87) "HOLD" else if (class_roll < 0.93) "COMP" else "GROUP"

    if (cls == "SEASON") {
      status <- "SOLD"
      add_dt <- format(as.POSIXct("2025-10-01 10:00:00", tz = "America/New_York") + runif(1, 0, 30*86400), "%Y-%m-%d %H:%M:%OS3")
    } else if (runif(1) < 0.40 & cls == "DIST-OPEN") {
      status <- "SOLD"
      sale_start <- as.POSIXct("2025-11-15 09:00:00", tz = "America/New_York")
      sale_end <- as.POSIXct(paste(ev_date, "00:00:00"), tz = "America/New_York")
      add_dt <- format(sale_start + runif(1, 0, as.numeric(sale_end - sale_start, units="secs")), "%Y-%m-%d %H:%M:%OS3")
    } else if (runif(1) < 0.1) {
      status <- "COMP"; add_dt <- format(as.POSIXct("2026-01-01 10:00:00", tz="America/New_York") + runif(1,0,90*86400), "%Y-%m-%d %H:%M:%OS3")
    } else if (cls %in% c("HOLD","GROUP","COMP")) {
      status <- "HELD"; add_dt <- NA_character_
    } else {
      status <- "available"; add_dt <- NA_character_
    }

    rows[[s]] <- list(event_name=ev, section_name=as.character(sec), row_name=as.character(rw),
                      seat_num=seat, num_seats=1L, class_name=cls, status=status, add_datetime=add_dt)
  }
  bind_rows(rows)
}

inv_parts <- list()
for (ev in focus_events) {
  ev_date <- event_schedule$event_date[event_schedule$event_id == ev]
  inv_parts[[length(inv_parts)+1]] <- generate_event_inventory(ev, ev_date, 5000)
}
for (ev in sample(setdiff(all_event_ids, focus_events), 6)) {
  ev_date <- event_schedule$event_date[event_schedule$event_id == ev]
  inv_parts[[length(inv_parts)+1]] <- generate_event_inventory(ev, ev_date, 500)
}

inventory <- bind_rows(inv_parts)
write.csv(inventory, file.path(out_dir, "inventory.csv"), row.names = FALSE)

# ── 7. CLICKSTREAM ───────────────────────────────────────────────────────────

cat("  clickstream.csv\n")

n_visits <- 12000
click_rows <- list()
row_idx <- 1
n_purchases <- round(n_visits * 0.035)
purchase_order_keys <- paste0("38-", sprintf("%05d", sample(10000:99999, n_purchases)))
purchase_counter <- 0

for (v in 1:n_visits) {
  ev_idx <- sample(1:nrow(event_schedule), 1)
  ev <- event_schedule$event_id[ev_idx]
  ev_date <- event_schedule$event_date[ev_idx]
  opp <- event_schedule$opponent[ev_idx]

  days_before <- sample(1:90, 1, prob = exp(-0.03 * 1:90))
  visit_date <- ev_date - days(days_before)
  if (visit_date < as.Date("2026-01-01")) visit_date <- as.Date("2026-01-01")

  visit_hour <- sample(8:23, 1, prob = c(1,1,2,3,4,5,6,7,8,8,7,5,4,3,2,1))
  visit_start <- as.POSIXct(
    paste(visit_date, sprintf("%02d:%02d:%02d", visit_hour, sample(0:59,1), sample(0:59,1))),
    tz = "America/New_York"
  )

  visitor_id <- paste0(rand_id(), "-", rand_id(), "-Web")
  session_num <- sample(1:10, 1)
  visit_id <- paste0(visitor_id, "-", session_num, "-", as.integer(visit_start))
  n_hits <- sample(2:8, 1, prob = c(4,3,2,1,1,1,1))
  is_purchase <- (purchase_counter < n_purchases) && (runif(1) < 0.04)
  if (is_purchase) purchase_counter <- purchase_counter + 1

  product_list <- paste0(
    "Atlanta Braves;Atlanta Braves vs. ", opp, " on ", format(ev_date, "%m/%d/%Y"),
    ";;;;106=::hash::0|147=::hash::0|1335=::hash::0"
  )

  for (h in 1:n_hits) {
    hit_time <- visit_start + (h-1) * sample(15:180, 1)
    hit_id <- paste0(rand_id(), rand_id())

    if (h < n_hits || !is_purchase) {
      pagename <- "Ticketmaster: Event Detail"
      pid <- NA_character_
    } else {
      pagename <- "Ticketmaster: Confirmation"
      ok <- purchase_order_keys[purchase_counter]
      pid <- paste0("ATL:TM_", ok, "/ATL-", format(visit_date, "%m%d%y"), sample(100:999,1))
    }

    click_rows[[row_idx]] <- list(
      hit_id=hit_id, hit_timestamp=format(hit_time, "%Y-%m-%d %H:%M:%S"),
      visitor_id=visitor_id, visit_id=visit_id, pagename=pagename,
      product_list=product_list, purchase_id=pid, event_name=ev,
      event_date=as.character(ev_date), season_year=2026L,
      game_number=ev_idx, season_id=569L
    )
    row_idx <- row_idx + 1
  }
}

clickstream <- bind_rows(click_rows)
write.csv(clickstream, file.path(out_dir, "clickstream.csv"), row.names = FALSE)

# ── 8. GRANDSTAND ORDERS ────────────────────────────────────────────────────

cat("  grandstand_orders.csv\n")

purchases <- clickstream %>%
  filter(!is.na(purchase_id)) %>%
  mutate(order_key = str_extract(purchase_id, "(?<=ATL:TM_)[^/]+")) %>%
  distinct(order_key, event_date, event_name)

gs_rows <- vector("list", nrow(purchases))
for (i in 1:nrow(purchases)) {
  ok <- purchases$order_key[i]
  ev_date <- as.Date(purchases$event_date[i])
  m_row <- manifest[sample(1:nrow(manifest), 1), ]
  num_seats <- sample(1:6, 1, prob = c(1,4,3,2,1,1))
  first_seat <- sample(1:20, 1)
  tp <- price_tiers$base_price[price_tiers$pc_family == m_row$pc_family]
  if (length(tp)==0) tp <- 50
  ticket_price <- round(tp * runif(1, 0.8, 1.3), 2)

  gs_rows[[i]] <- list(
    order_id = paste0("tm_5_", ok, "/atl_2026_", sample(1:10,1)),
    order_key = ok, transaction_date = paste(ev_date - sample(0:30,1), sprintf("%02d:%02d:%02d", sample(9:21,1), sample(0:59,1), sample(0:59,1))),
    event_date = as.character(ev_date), game_pk = 800000L + i,
    section = m_row$section_name, row = m_row$row_name,
    first_seat = first_seat, last_seat = first_seat + num_seats - 1,
    num_seats = num_seats, ticket_price = ticket_price,
    total_price = round(ticket_price * num_seats, 2),
    pc_family = m_row$pc_family, price_location = m_row$price_location
  )
}

grandstand_orders <- bind_rows(gs_rows)
write.csv(grandstand_orders, file.path(out_dir, "grandstand_orders.csv"), row.names = FALSE)

# ── 9. VISIT EVENT SUMMARY (modeling input) ─────────────────────────────────

cat("  visit_event_summary.csv\n")

n_summary <- 50000

visitor_ids <- paste0(rand_id(n = n_summary), "-",
                      rand_id(n = n_summary), "-Web")
visit_ids <- paste0(visitor_ids, "-", sample(1:15, n_summary, replace=TRUE), "-",
                    sample(1770000000:1780000000, n_summary))

ev_indices <- sample(1:nrow(event_schedule), n_summary, replace=TRUE)
ev_names <- event_schedule$event_id[ev_indices]
ev_dates <- event_schedule$event_date[ev_indices]
ev_scores_vec <- event_schedule$EVENTSCORE[ev_indices]

days_before <- pmax(0, round(rexp(n_summary, rate=0.04)))
days_before <- pmin(days_before, 120)
hit_dates <- ev_dates - days(days_before)
hit_hours <- sample(8:23, n_summary, replace=TRUE, prob=c(1,1,2,3,4,5,6,7,8,8,7,5,4,3,2,1))
hit_times <- as.POSIXct(paste(hit_dates, sprintf("%02d:%02d:%02d", hit_hours, sample(0:59,n_summary,replace=TRUE), sample(0:59,n_summary,replace=TRUE))),
                         tz="America/New_York")

base_dist_open <- sample(2000:4500, n_summary, replace=TRUE)
dist_open_count <- pmax(200, round(base_dist_open * (0.3 + 0.7 * days_before / 90) + rnorm(n_summary, 0, 200)))
cheapest_price <- round(pmax(12, 25 + (dist_open_count - 2500) * -0.008 + rnorm(n_summary, 0, 10)), 2)
avg_price <- round(cheapest_price + runif(n_summary, 20, 65), 2)
priciest_price <- round(avg_price + runif(n_summary, 30, 120), 2)
cheapest_pc_fam <- sample(LETTERS[10:16], n_summary, replace=TRUE)
priciest_pc_fam <- sample(LETTERS[1:5], n_summary, replace=TRUE)

# Purchase probability model (realistic with noise)
logit_p <- -3.5 +
  -0.008 * cheapest_price +
  0.006 * ev_scores_vec +
  -0.015 * days_before +
  0.5 * (days_before < 3) +
  -0.0002 * dist_open_count +
  0.03 * (hit_hours >= 18) +
  rnorm(n_summary, 0, 0.8)

prob_purchase <- 1 / (1 + exp(-logit_p))
purchased <- runif(n_summary) < prob_purchase
cat("    Purchase rate:", round(mean(purchased)*100, 1), "%\n")

purchase_id <- ifelse(purchased,
  paste0("ATL:TM_38-", sprintf("%05d", sample(10000:99999, n_summary, replace=TRUE)),
         "/ATL-", format(hit_dates, "%m%d%y"), sample(100:999, n_summary, replace=TRUE)),
  NA_character_)

section <- ifelse(purchased, as.character(sample(c(101:120, 201:220, 301:320, 401:410), n_summary, replace=TRUE)), NA_character_)
row_num <- ifelse(purchased, as.character(sample(1:15, n_summary, replace=TRUE)), NA_character_)
price_location <- ifelse(purchased, sample(price_tiers$price_location, n_summary, replace=TRUE), NA_character_)
num_seats <- ifelse(purchased, sample(1:6, n_summary, replace=TRUE, prob=c(1,4,3,2,1,1)), NA_integer_)
ticket_price <- ifelse(purchased, round(cheapest_price + runif(n_summary, -5, 40), 2), NA_real_)
total_price <- ifelse(purchased, round(ticket_price * num_seats, 2), NA_real_)

visit_event_summary <- tibble(
  visitor_id=visitor_ids, visit_id=visit_ids, event_name=ev_names,
  event_date=as.character(ev_dates), first_hit_timestamp=format(hit_times, "%Y-%m-%d %H:%M:%S"),
  purchase_id=purchase_id, purchased=purchased, section=section, row=row_num,
  price_location=price_location, num_seats=num_seats, ticket_price=ticket_price,
  total_price=total_price, dist_open_count=dist_open_count, cheapest_price=cheapest_price,
  cheapest_pc_family=cheapest_pc_fam, priciest_price=priciest_price,
  priciest_pc_family=priciest_pc_fam, avg_price=avg_price
)

write.csv(visit_event_summary, file.path(out_dir, "visit_event_summary.csv"), row.names = FALSE)

# ── 10. SALES BY DATE ───────────────────────────────────────────────────────

cat("  sales_by_date.csv\n")

sales_rows <- list()
row_idx <- 1
for (i in 1:nrow(event_schedule)) {
  ev <- event_schedule$event_id[i]
  ev_date <- event_schedule$event_date[i]
  sale_start <- max(ev_date - days(120), as.Date("2025-11-10"))
  sale_dates <- seq(sale_start, ev_date, by="day")
  sale_dates <- sale_dates[runif(length(sale_dates)) < 0.6]

  for (sd in sale_dates) {
    sd <- as.Date(sd, origin="1970-01-01")
    days_out <- as.integer(ev_date - sd)
    base_trans <- max(1, round(3 + 15 * exp(-0.05 * days_out) + rnorm(1, 0, 2)))
    sales_rows[[row_idx]] <- list(
      event_name=ev, event_date=as.character(ev_date), add_date=as.character(sd),
      unique_buyers=max(1, base_trans - sample(0:2,1)), transactions=base_trans,
      num_seats=round(base_trans * runif(1, 1.8, 3.2)),
      total_revenue=round(base_trans * runif(1, 60, 180), 2)
    )
    row_idx <- row_idx + 1
  }
}

sales_by_date <- bind_rows(sales_rows)
write.csv(sales_by_date, file.path(out_dir, "sales_by_date.csv"), row.names = FALSE)

# ── SUMMARY ──────────────────────────────────────────────────────────────────

cat("\n=== Synthetic data generation complete ===\n")
cat("Files written to:", out_dir, "\n\n")
for (f in list.files(out_dir, pattern="\\.csv$")) {
  n <- nrow(read.csv(file.path(out_dir, f), nrows=-1))
  cat(sprintf("  %-35s %6d rows\n", f, n))
}

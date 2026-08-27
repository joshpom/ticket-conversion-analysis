# ══════════════════════════════════════════════════════════════════════════════
# 02 — Production-Scale Site Traffic Pipeline
# ══════════════════════════════════════════════════════════════════════════════
#
# PURPOSE:
#   Scaled, vectorized version of the snapshot engine (Script 01) that processes
#   ALL ~80 games in a single pass. This is the production pipeline that feeds
#   the modeling script.
#
# IMPROVEMENTS OVER SCRIPT 01:
#   - All events processed at once (no per-event function calls for intervals)
#   - Vectorized price/class interval construction with group_by(event_name)
#   - Vectorized DIST-OPEN history calculation
#   - Single data.table rolling join across all events
#   - Cleaner purchase linking and grandstand order joining
#
# INPUT:
#   data/change_journal_price.csv, data/change_journal_reclass.csv,
#   data/clickstream.csv, data/inventory.csv, data/manifest.csv,
#   data/grandstand_orders.csv
#
# OUTPUT:
#   output/site_hits_w_purchases_and_snapshots.csv
#
# ══════════════════════════════════════════════════════════════════════════════

library(dplyr)
library(stringr)
library(purrr)
library(data.table)

# ── LOAD DATA ────────────────────────────────────────────────────────────────

manifest <- read.csv("data/manifest.csv", stringsAsFactors = FALSE) %>%
  mutate(section_name = as.character(section_name),
         row_name     = as.character(row_name)) %>%
  distinct(section_name, row_name, pc_family, price_location)

cat("Pulling journal data...\n")
cj_reclass <- read.csv("data/change_journal_reclass.csv", stringsAsFactors = FALSE) %>%
  mutate(upd_datetime = as.POSIXct(upd_datetime, tz = "America/New_York"))

change_journal <- read.csv("data/change_journal_price.csv", stringsAsFactors = FALSE) %>%
  mutate(upd_datetime = as.POSIXct(upd_datetime, tz = "America/New_York"))

cat("Pulling clickstream...\n")
full_clickstream <- read.csv("data/clickstream.csv", stringsAsFactors = FALSE) %>%
  mutate(
    hit_timestamp = as.POSIXct(hit_timestamp, tz = "America/New_York"),
    event_date    = as.Date(event_date)
  )

cat("Pulling inventory...\n")
inventory <- read.csv("data/inventory.csv", stringsAsFactors = FALSE) %>%
  mutate(add_datetime = as.POSIXct(add_datetime, tz = "America/New_York"),
         section_name = as.character(section_name),
         row_name     = as.character(row_name))

cat("Pulling grandstand orders...\n")
grandstand_orders <- read.csv("data/grandstand_orders.csv", stringsAsFactors = FALSE) %>%
  mutate(
    transaction_date = as.POSIXct(transaction_date, tz = "America/New_York"),
    event_date       = as.Date(event_date),
    section          = as.character(section),
    row              = as.character(row)
  )

# ── HELPERS ──────────────────────────────────────────────────────────────────

INF_TIME <- as.POSIXct("2099-12-31 23:59:59", tz = "America/New_York")
events   <- unique(cj_reclass$event_name)

# ── BUILD PRICE INTERVALS (all events at once) ──────────────────────────────
# Vectorized: no per-event function calls.

cat("Building price intervals...\n")

insert_prices <- change_journal %>%
  filter(action_name == "Insert", nchar(price_code) == 1) %>%
  mutate(price = as.numeric(map_chr(new_value, ~ str_split(.x, "\\|")[[1]][1]))) %>%
  select(event_name, price_code, upd_datetime, price)

update_prices <- change_journal %>%
  filter(action_name == "Update", column_name == "price", nchar(price_code) == 1) %>%
  mutate(price = as.numeric(new_value)) %>%
  select(event_name, price_code, upd_datetime, price)

delete_times <- change_journal %>%
  filter(action_name == "Delete", nchar(price_code) == 1) %>%
  select(event_name, price_code, deleted_at = upd_datetime)

price_intervals <- bind_rows(insert_prices, update_prices) %>%
  arrange(event_name, price_code, upd_datetime) %>%
  left_join(delete_times, by = c("event_name", "price_code")) %>%
  group_by(event_name, price_code) %>%
  mutate(
    interval_start    = upd_datetime,
    interval_end      = lead(upd_datetime, default = INF_TIME),
    in_deleted_window = !is.na(deleted_at) &
      interval_start >= deleted_at &
      interval_start < suppressWarnings(min(upd_datetime[upd_datetime > deleted_at], na.rm = TRUE))
  ) %>%
  filter(!in_deleted_window) %>%
  ungroup() %>%
  select(event_name, price_code, price, interval_start, interval_end)

# ── BUILD CLASS INTERVALS (all events at once) ──────────────────────────────

cat("Building class intervals...\n")

cj_reclass_manifest <- cj_reclass %>%
  mutate(
    section_name = str_extract(seat_location, "^[^/]+"),
    row_name     = str_extract(seat_location, "(?<=/)[^/]+(?=/)")
  ) %>%
  left_join(manifest %>% select(section_name, row_name, pc_family),
            by = c("section_name", "row_name"))

class_intervals <- cj_reclass_manifest %>%
  arrange(event_name, seat_location, upd_datetime) %>%
  group_by(event_name, seat_location) %>%
  mutate(
    class          = old_value,
    interval_start = lag(upd_datetime),
    interval_end   = upd_datetime
  ) %>%
  filter(!is.na(interval_start)) %>%
  ungroup() %>%
  bind_rows(
    cj_reclass_manifest %>%
      arrange(event_name, seat_location, upd_datetime) %>%
      group_by(event_name, seat_location) %>%
      slice_max(upd_datetime, n = 1, with_ties = FALSE) %>%
      ungroup() %>%
      mutate(
        class          = new_value,
        interval_start = upd_datetime,
        interval_end   = INF_TIME
      ) %>%
      select(event_name, seat_location, section_name, row_name, pc_family,
             class, interval_start, interval_end)
  ) %>%
  filter(interval_start < interval_end) %>%
  select(event_name, seat_location, section_name, row_name, pc_family,
         class, interval_start, interval_end) %>%
  arrange(event_name, seat_location, interval_start)

# ── BUILD DIST-OPEN HISTORY (vectorized, all events) ────────────────────────

cat("Building DIST-OPEN history...\n")

dt_class <- as.data.table(class_intervals)

# Current available DIST-OPEN count per event
current_counts <- inventory %>%
  filter(!status %in% c("SOLD", "COMP"), class_name == "DIST-OPEN") %>%
  group_by(event_name) %>%
  summarise(current_count = sum(num_seats), .groups = "drop")

# Sold seats with class at time of sale via non-equi join
sold <- inventory %>%
  filter(status %in% c("SOLD", "COMP"), !is.na(add_datetime)) %>%
  select(event_name, section_name, row_name, seat_num, num_seats, add_datetime)

dt_sold <- as.data.table(sold)

sold_with_class <- dt_class[
  dt_sold,
  .(event_name = i.event_name, section_name, row_name, seat_num,
    num_seats, add_datetime, class),
  on = .(event_name = event_name,
         section_name = section_name,
         row_name = row_name,
         interval_start <= add_datetime,
         interval_end > add_datetime),
  nomatch = NA
] %>% as_tibble()

# Reclass changes for backwards adjustment
reclass_changes <- cj_reclass %>%
  mutate(
    section_name = str_extract(seat_location, "^[^/]+"),
    row_name     = str_extract(seat_location, "(?<=/)[^/]+(?=/)")
  ) %>%
  select(event_name, old_value, new_value, upd_datetime)

# All change times per event
change_times_all <- bind_rows(
  reclass_changes %>% select(event_name, t = upd_datetime),
  sold_with_class %>% select(event_name, t = add_datetime)
) %>%
  distinct() %>%
  arrange(event_name, t)

# Backwards calculation per event
dist_open_history <- change_times_all %>%
  left_join(current_counts, by = "event_name") %>%
  group_by(event_name) %>%
  mutate(
    dist_open_count = map_int(t, function(ts) {
      ev <- event_name[1]
      rc <- reclass_changes %>% filter(event_name == ev, upd_datetime > ts)
      into  <- sum(rc$new_value == "DIST-OPEN")
      outof <- sum(rc$old_value == "DIST-OPEN")
      sales <- sold_with_class %>%
        filter(event_name == ev, add_datetime > ts, class == "DIST-OPEN") %>%
        summarise(n = sum(num_seats)) %>% pull(n)
      current_count[1] - into + outof + sales
    })
  ) %>%
  ungroup() %>%
  select(event_name, hit_timestamp = t, dist_open_count) %>%
  as.data.table()

# ── COMPUTE PRICE SNAPSHOTS (all events) ─────────────────────────────────────

cat("Computing price snapshots...\n")

dt_price <- as.data.table(price_intervals)

price_change_times <- bind_rows(
  price_intervals %>% select(event_name, t = interval_start),
  class_intervals  %>% select(event_name, t = interval_start)
) %>% distinct() %>% arrange(event_name, t)

price_snapshots <- price_change_times %>%
  group_by(event_name) %>%
  mutate(
    cheapest_price = map_dbl(t, function(ts) {
      ev   <- event_name[1]
      fams <- dt_class[event_name == ev & interval_start <= ts & interval_end > ts &
                         class == "DIST-OPEN" & !is.na(pc_family), unique(pc_family)]
      p    <- dt_price[event_name == ev & interval_start <= ts & interval_end > ts &
                         price_code %in% fams]
      if (nrow(p)) p[which.min(price), price] else NA_real_
    }),
    cheapest_pc_family = map_chr(t, function(ts) {
      ev   <- event_name[1]
      fams <- dt_class[event_name == ev & interval_start <= ts & interval_end > ts &
                         class == "DIST-OPEN" & !is.na(pc_family), unique(pc_family)]
      p    <- dt_price[event_name == ev & interval_start <= ts & interval_end > ts &
                         price_code %in% fams]
      if (nrow(p)) p[which.min(price), price_code] else NA_character_
    }),
    priciest_price = map_dbl(t, function(ts) {
      ev   <- event_name[1]
      fams <- dt_class[event_name == ev & interval_start <= ts & interval_end > ts &
                         class == "DIST-OPEN" & !is.na(pc_family), unique(pc_family)]
      p    <- dt_price[event_name == ev & interval_start <= ts & interval_end > ts &
                         price_code %in% fams]
      if (nrow(p)) p[which.max(price), price] else NA_real_
    }),
    priciest_pc_family = map_chr(t, function(ts) {
      ev   <- event_name[1]
      fams <- dt_class[event_name == ev & interval_start <= ts & interval_end > ts &
                         class == "DIST-OPEN" & !is.na(pc_family), unique(pc_family)]
      p    <- dt_price[event_name == ev & interval_start <= ts & interval_end > ts &
                         price_code %in% fams]
      if (nrow(p)) p[which.max(price), price_code] else NA_character_
    }),
    avg_price = map_dbl(t, function(ts) {
      ev   <- event_name[1]
      fams <- dt_class[event_name == ev & interval_start <= ts & interval_end > ts &
                         class == "DIST-OPEN" & !is.na(pc_family),
                       .(seat_count = .N), by = pc_family]
      p    <- dt_price[event_name == ev & interval_start <= ts & interval_end > ts &
                         price_code %in% fams$pc_family]
      if (nrow(p) == 0) return(NA_real_)
      m <- merge(p, fams, by.x = "price_code", by.y = "pc_family")
      sum(m$price * m$seat_count) / sum(m$seat_count)
    })
  ) %>%
  ungroup() %>%
  rename(hit_timestamp = t) %>%
  as.data.table()

# ── ROLLING JOIN TO CLICKSTREAM ──────────────────────────────────────────────

cat("Rolling join to clickstream...\n")

dt_hits <- full_clickstream %>%
  filter(!is.na(event_name)) %>%
  select(hit_id, hit_timestamp, event_name) %>%
  arrange(event_name, hit_timestamp) %>%
  as.data.table()

setkey(dt_hits,           event_name, hit_timestamp)
setkey(dist_open_history, event_name, hit_timestamp)
setkey(price_snapshots,   event_name, hit_timestamp)

dist_joined  <- dist_open_history[dt_hits,  roll = TRUE, on = .(event_name, hit_timestamp)]
price_joined <- price_snapshots[dt_hits,    roll = TRUE, on = .(event_name, hit_timestamp)]

snapshots_all <- dist_joined[
  price_joined,
  on = .(event_name, hit_timestamp, hit_id)
] %>% as_tibble()

# ── JOIN SNAPSHOTS BACK TO FULL CLICKSTREAM ──────────────────────────────────

result <- full_clickstream %>%
  left_join(snapshots_all, by = c("hit_id", "event_name", "hit_timestamp"))

cat("Rows in result:", nrow(result), "\n")

# ── LINK PURCHASES TO EVENT DETAIL HITS ──────────────────────────────────────

cat("Linking purchases...\n")

event_detail_hits <- full_clickstream %>%
  filter(pagename == "Ticketmaster: Event Detail",
         str_detect(product_list, "\\d{1,2}/\\d{1,2}/\\d{4}")) %>%
  mutate(
    event_date = as.Date(str_extract(product_list, "\\d{1,2}/\\d{1,2}/\\d{4}"),
                         format = "%m/%d/%Y")
  )

confirmations <- full_clickstream %>%
  filter(str_detect(pagename, "Confirmation"),
         !is.na(purchase_id),
         str_detect(product_list, " vs\\.| v\\.")) %>%
  select(visit_id, purchase_id, hit_timestamp)

result_linked <- event_detail_hits %>%
  select(-purchase_id) %>%
  inner_join(
    confirmations %>% select(visit_id, purchase_id, confirmation_time = hit_timestamp),
    by = "visit_id", relationship = "many-to-many"
  ) %>%
  filter(hit_timestamp <= confirmation_time) %>%
  group_by(visit_id, purchase_id, confirmation_time) %>%
  slice_max(hit_timestamp, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(hit_id, visit_id, purchase_id, event_date)

purchase_event_date <- result_linked %>%
  select(visit_id, purchase_id, event_date)

# ── BUILD FINAL DATASET ─────────────────────────────────────────────────────

cat("Building final dataset...\n")

visits_with_date <- full_clickstream %>%
  filter(str_detect(product_list, "\\d{1,2}/\\d{1,2}/\\d{4}")) %>%
  distinct(visit_id)

visits_final <- full_clickstream %>%
  filter(visit_id %in% visits_with_date$visit_id) %>%
  left_join(result_linked %>% select(hit_id, purchase_id_linked = purchase_id,
                                     event_date_linked = event_date),
            by = "hit_id") %>%
  left_join(purchase_event_date %>% rename(event_date_from_purchase = event_date),
            by = c("visit_id", "purchase_id")) %>%
  mutate(event_date = coalesce(event_date_linked, event_date_from_purchase)) %>%
  select(-event_date_linked, -event_date_from_purchase) %>%
  # Extract order key from purchase_id (ATL:TM_ prefix format)
  mutate(
    order_key = case_when(
      str_detect(purchase_id, "/") ~ str_sub(purchase_id, 8, 15),
      !is.na(purchase_id)          ~ str_extract(purchase_id, "(?<=ATL:TM_)\\d{8}")
    )
  ) %>%
  # Join grandstand order details
  left_join(grandstand_orders %>% select(-event_date), by = "order_key") %>%
  # Join snapshots
  left_join(
    result %>% select(hit_id, dist_open_count, cheapest_price, cheapest_pc_family,
                      priciest_price, priciest_pc_family, avg_price),
    by = "hit_id"
  ) %>%
  arrange(visit_id, hit_timestamp)

cat("Rows in visits_final:", nrow(visits_final), "\n")

# ── WRITE OUTPUT ─────────────────────────────────────────────────────────────

dir.create("output", showWarnings = FALSE)
write.csv(visits_final, "output/site_hits_w_purchases_and_snapshots.csv", row.names = FALSE)
cat("Done.\n")

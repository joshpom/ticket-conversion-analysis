# ══════════════════════════════════════════════════════════════════════════════
# 01 — Change Journal Processing: Point-in-Time Price Snapshot
# ══════════════════════════════════════════════════════════════════════════════
#
# PURPOSE:
#   Reconstruct the active price codes and their prices for a single game at a
#   specific point in time, using the ticketing system's audit log (the "change
#   journal").  The change journal records every Insert, Update, and Delete to
#   price codes, so by replaying it up to a chosen timestamp we can see exactly
#   what prices were live on the website at that moment.
#
# APPROACH:
#   1. Filter to single-character price codes (the per-seat pricing tiers).
#   2. Find the most recent price Update at or before the snapshot timestamp
#      to get the current price and its predecessor.
#   3. Fall back to the original Insert record for price codes that were never
#      updated — the price is embedded as the first pipe-delimited field in the
#      Insert blob.
#   4. Remove any price codes whose last action before the snapshot was a Delete.
#   5. Merge into a single snapshot table: (price_code, previous_price,
#      current_price, last_updated).
#
# INPUT:
#   data/change_journal_price.csv — price code change journal for all games
#
# OUTPUT:
#   Console printout of the active price codes and prices at the snapshot time.
#
# ══════════════════════════════════════════════════════════════════════════════

library(dplyr)
library(stringr)
library(tidyr)
library(purrr)

# ── LOAD DATA ────────────────────────────────────────────────────────────────

change_journal_full <- read.csv("data/change_journal_price.csv",
                                stringsAsFactors = FALSE) %>%
  mutate(upd_datetime = as.POSIXct(upd_datetime, tz = "America/New_York"))

# ── CONFIGURATION ────────────────────────────────────────────────────────────

target_event  <- "E6BB0424"
snapshot_date <- as.POSIXct("2026-04-21 04:36:12", tz = "America/New_York")

# ── FILTER TO TARGET EVENT AND SINGLE-CHARACTER PRICE CODES ──────────────────
# Multi-character codes are composite / group-level — we want per-seat tiers.

journal <- change_journal_full %>%
  filter(event_name == target_event,
         nchar(price_code) == 1)

# ── CURRENT PRICE: latest price Update at/before snapshot ────────────────────
# Each Update row records the old and new price for one price code.

latest_price_update <- journal %>%
  filter(action_name == "Update",
         column_name  == "price",
         upd_datetime <= snapshot_date) %>%
  group_by(price_code) %>%
  slice_max(upd_datetime, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(price_code,
         previous_price = old_value,
         current_price  = new_value,
         last_updated   = upd_datetime)

# ── FALLBACK: price codes never updated — extract price from Insert blob ─────
# Insert rows pack all fields into a single pipe-delimited string.  The first
# field is the ticket price.

inserted_prices <- journal %>%
  filter(action_name == "Insert",
         upd_datetime <= snapshot_date) %>%
  group_by(price_code) %>%
  slice_max(upd_datetime, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(current_price = map_chr(new_value, ~ str_split(.x, "\\|")[[1]][1])) %>%
  select(price_code, current_price, last_updated = upd_datetime)

# ── DELETED PRICE CODES ──────────────────────────────────────────────────────
# If the last action before the snapshot was a Delete, the code was inactive.

deleted_codes <- journal %>%
  filter(upd_datetime <= snapshot_date) %>%
  group_by(price_code) %>%
  slice_max(upd_datetime, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(action_name == "Delete") %>%
  pull(price_code)

# ── COMBINE ──────────────────────────────────────────────────────────────────
# Updated codes take priority for current_price; insert-only codes fill in.
# previous_price is NA for codes that were never updated.

snapshot <- inserted_prices %>%
  left_join(latest_price_update, by = "price_code", suffix = c("_insert", "_update")) %>%
  mutate(
    current_price = coalesce(current_price_update, current_price_insert),
    last_updated  = coalesce(last_updated_update,  last_updated_insert)
  ) %>%
  select(price_code, previous_price, current_price, last_updated) %>%
  filter(!price_code %in% deleted_codes) %>%
  arrange(price_code)

# ── RESULTS ──────────────────────────────────────────────────────────────────

cat("Price snapshot as of:", format(snapshot_date), "\n")
cat("Active price codes:  ", nrow(snapshot), "\n\n")
print(snapshot, n = 31)

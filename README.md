# Dynamic Ticket Pricing & Purchase Decision Modeling

**How do real-time price changes and seat availability affect whether a website visitor buys a ticket?**

This project builds a complete analytical pipeline — from raw ticketing-system audit logs to calibrated machine-learning models — to quantify the causal drivers of online ticket purchases for the Atlanta Braves' 2026 MLB season.

> **Note:** All data in this repository is **synthetic**. The analytical methods were developed against real production data, but every dataset here was generated from scratch to protect proprietary information. Run `data/generate_synthetic_data.R` to reproduce the synthetic data.

---

## Pipeline Overview

```
┌─────────────────────────┐     ┌────────────────────────────────┐
│  Price Code Change Log  │     │  Seat Reclassification Log     │
│  (Insert/Update/Delete) │     │  (class transitions over time) │
└───────────┬─────────────┘     └───────────────┬────────────────┘
            │                                   │
            ▼                                   ▼
   ┌─────────────────────────────────────────────────────┐
   │  01 — Change Journal Processing                     │
   │  Reconstruct point-in-time price snapshots from     │
   │  the Archtics ticketing system's audit trail        │
   └───────────────────────┬─────────────────────────────┘
                           │
            ┌──────────────┴──────────────┐
            ▼                             ▼
  ┌──────────────────────┐    ┌───────────────────────────────┐
  │  02 — Snapshot Engine │    │  03 — Production Pipeline      │
  │  Prototype: 2 games   │    │  All ~80 games, vectorized     │
  │  + ggplot dashboards  │    │  data.table non-equi joins     │
  └──────────┬───────────┘    └───────────────┬───────────────┘
             │                                │
             └────────────┬───────────────────┘
                          ▼
              ┌───────────────────────────┐     ┌──────────────────┐
              │  Visit-Event Summary      │◄────│  DA Event Demand │
              │  (one row per session ×   │     │  Scores          │
              │   game, with snapshots)   │     └──────────────────┘
              └───────────┬───────────────┘
                          ▼
              ┌───────────────────────────────────────────────┐
              │  04 — Purchase Decision Modeling               │
              │  XGBoost + Keras neural network ensemble       │
              │  Visitor segmentation & K-means clustering     │
              │  Calibrated partial dependence analysis        │
              └───────────────────────────────────────────────┘
```

## Scripts

| # | Script | Purpose | Key Techniques |
|---|--------|---------|----------------|
| 01 | `01_change_journal_processing.R` | Reconstruct point-in-time price snapshots from the Archtics change journal | Pipe-delimited field parsing, temporal replay of Insert/Update/Delete actions |
| 02 | `02_snapshot_engine.R` | Build temporal price and inventory snapshots for 2 games; generate tracking dashboards; link website visits to purchases | `data.table` non-equi rolling joins, backwards inventory calculation, `ggplot2` + `patchwork` dashboards |
| 03 | `03_site_traffic_pipeline.R` | Production-scale version of Script 02 for all ~80 games | Vectorized interval construction, scalable `data.table` pipeline, multi-table purchase linking |
| 04 | `04_purchase_decision_modeling.R` | Model purchase probability from game-level and visitor-level features | XGBoost, Keras, Platt-scaled partial dependence, K-means visitor clustering, high/low demand segmentation |

## Key Concepts

| Term | Meaning |
|------|---------|
| **DIST-OPEN** | Archtics ticket class for seats available for general public sale on Ticketmaster |
| **Price code (pc_family)** | Single-character code mapping a seat location to a pricing tier |
| **Change journal** | Archtics audit log tracking every Insert, Update, and Delete to price codes and seat classifications |
| **DA Price Location** | Demand Analytics grouping of seats by location and pricing tier |
| **Rolling join** | `data.table` operation that matches each website visit timestamp to the most recent inventory/price snapshot |
| **Visit-event summary** | One row per visitor session × game — the unit of analysis for modeling |

## Key Findings

1. **Price sensitivity depends on demand tier:**
   - For **high-demand games**, cheapest available price is the #1 predictor of purchase
   - For **low-demand games**, time-of-day and days-before-game matter more than price

2. **Visitor segments behave differently:** K-means clustering identifies 5 distinct visitor types (High Intent Repeat Browsers, Last Minute Browsers, Early Browsers, Premium Game Browsers, Active Session Browsers), each with different conversion drivers

3. **Calibrated price effect:** Each $1 increase in cheapest available price reduces purchase probability by ~0.025 percentage points (calibrated via Platt scaling on XGBoost partial dependence)

## Setup

### Requirements

```r
install.packages(c(
  "tidyverse", "data.table", "xgboost", "keras", "patchwork",
  "pdp", "pROC", "scales", "lubridate", "cluster",
  "factoextra", "ggrepel"
))
```

Keras requires a Python backend — see the [keras R package documentation](https://keras.posit.co/) for setup.

### Generate Synthetic Data

```r
source("data/generate_synthetic_data.R")
```

This creates all CSV files in `data/` needed by the four analysis scripts.

### Run the Pipeline

```r
source("01_change_journal_processing.R")   # explore a single-game price snapshot
source("02_snapshot_engine.R")             # prototype + dashboards for 2 games
source("03_site_traffic_pipeline.R")       # full-season production pipeline
source("04_purchase_decision_modeling.R")  # ML models + clustering
```

Scripts 02 and 03 overlap in scope — 02 is the exploratory prototype (with visualizations), 03 is the scalable production version.

## Data

All data is synthetic. Run `data/generate_synthetic_data.R` to regenerate.

| File | Rows | Description |
|------|------|-------------|
| `manifest.csv` | ~450 | Venue seating map (section/row → price tier) |
| `event_schedule.csv` | ~80 | Season game schedule with opponents |
| `event_scores.csv` | ~80 | DA game demand/attractiveness ratings |
| `change_journal_price.csv` | ~25K | Price code audit log (all games) |
| `change_journal_reclass.csv` | ~2K | Seat reclassification audit log |
| `inventory.csv` | ~10K | Seat inventory snapshot (focus games) |
| `clickstream.csv` | ~50K | Ticketmaster page views with visit/session structure |
| `grandstand_orders.csv` | ~4K | Ticket order details |
| `visit_event_summary.csv` | ~50K | Aggregated visit × game data (modeling input) |
| `sales_by_date.csv` | ~3K | Daily sales aggregations per game |

## Author

Josh Pomerantz

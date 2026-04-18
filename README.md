# Predictive Analysis of Video Game Commercial Sales Performance: A Data-Driven Study Using Regression and Classification Models

## Project Structure

```
project_root/
│
├── data/
│   ├── raw/
│   │   ├── console_sales_online.csv           # Combined online console-inclusive sales data
│   │   ├── vgchartz_raw.csv                   # Standardized core sales dataset
│   │   ├── publisher_market_sales_online.csv  # Publisher-year sales and market-share metrics
│   │   ├── publisher_market_share_template.csv # Publisher-year market share table for joins
│   │   ├── steam_api_raw.rds                  # Steam game attributes snapshot
│   │   ├── steam_reviews_raw.rds              # Steam review text snapshot
│   │   └── kaggle_vgsales.csv                 # Local copy/fallback of standardized sales data
│   ├── processed/
│       ├── master_dataset.csv        # Final joined and encoded analysis-ready dataset
│       ├── sentiment_scores.csv      # Per-game aggregated sentiment metrics
│       ├── key_variable_overview.csv # Coverage/stats summary of key variables
│       └── publisher_overview.csv    # Publisher-level overview metrics
│   └── plots/
│       ├── games_by_genre_count.png
│       ├── platform_weighted_global_sales.png
│       ├── steam_review_percentage_trend.png
│       ├── games_by_release_year_count.png
│       ├── key_variables_missing_percentage.png
│       ├── key_variables_non_missing_count.png
│       ├── top_variables_missingness.png
│       ├── specific_genre_competition_overview.png
│       ├── top_publishers_total_global_sales.png
│       ├── publisher_sales_overview_bubble.png
│       └── top_publishers_market_share_trend.png
│
├── scripts/
│   ├── 01_data_scraping.R          # Collect raw data from VGChartz, Steam API, and Steam Reviews
│   ├── 02_data_curation.R          # Join, clean, encode variables; plot overviews to justify decisions
│   ├── 03_sentiment_analysis.R     # Score review text and append sentiment features to master dataset
│   ├── 04_pca.R                    # Reduce correlated predictors; visualise predictor structure
│   ├── 05_clustering.R             # Segment games into market tiers; append cluster as a predictor
│   ├── 06_regression.R             # Predict sales volume (linear) and commercial success (logistic) - Daniel
│   └── 07_cart.R                   # Validate regression findings; rank pre-launch attributes by importance
│
└── README.md
```

## Data Sources

This project uses a multi-source data pipeline combining online game-sales sources, Steam platform metadata, and repository-generated raw files.

### 1. Console-inclusive game sales (online)

- vgsales_andvise
	- https://raw.githubusercontent.com/andvise/DataAnalyticsDatasets/main/vgsales.csv
	- Used fields: title, platform, year, genre, publisher, regional sales, global sales.
- vgsales_saemaqazi
	- https://raw.githubusercontent.com/saemaqazi/vgsales.csv/main/vgsales.csv
	- Used fields: title, platform, year, genre, publisher, regional sales, global sales.
- vgchartz_2024
	- https://raw.githubusercontent.com/Bredmak/vgchartz-sales-analysis/main/vgchartz-2024.csv
	- Used fields (normalized): title, console/platform, release date/year, genre, publisher, regional sales, global sales.

### 2. Steam platform data (online, SteamKit workflow)

- Steam app search endpoint (appid resolution from game title)
	- https://steamcommunity.com/actions/SearchApps/{query}
	- Used fields: appid and app name matching for sales-title linkage.
- SteamKit-powered app info endpoint
	- https://api.steamcmd.net/v1/info/{appid}
	- Used fields: name, developer/publisher associations, review percentage, genres/tags identifiers, release date, and OS support.

Note: Steam review text API is intentionally disabled in this pipeline. The file [data/raw/steam_reviews_raw.rds](data/raw/steam_reviews_raw.rds) is currently an empty placeholder unless a dedicated non-synthetic review source is configured.

### 3. Raw data files generated in this repository

- [data/raw/console_sales_online.csv](data/raw/console_sales_online.csv)
	- Combined and standardized online console-inclusive sales records.
- [data/raw/vgchartz_raw.csv](data/raw/vgchartz_raw.csv)
	- Standardized sales base file used by curation.
- [data/raw/publisher_market_sales_online.csv](data/raw/publisher_market_sales_online.csv)
	- Publisher-year sales totals, game counts, and computed market share metrics.
- [data/raw/publisher_market_share_template.csv](data/raw/publisher_market_share_template.csv)
	- Publisher-year market share table used in joins during curation.
- [data/raw/steam_api_raw.rds](data/raw/steam_api_raw.rds)
	- Steam metadata snapshot.
- [data/raw/steam_reviews_raw.rds](data/raw/steam_reviews_raw.rds)
	- Steam review snapshot for sentiment feature construction.


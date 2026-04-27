# Predictive Analysis of Video Game Commercial Sales Performance: A Data-Driven Study Using Regression and Classification Models

## Project Structure

```
project_root/
│
├── data/
│   ├── raw/
│   │   ├── console_sales_online.csv
│   │   ├── kaggle_vgsales.csv
│   │   ├── publisher_market_sales_online.csv
│   │   ├── publisher_market_share_template.csv
│   │   ├── steam_api_raw.rds
│   │   ├── steam_game_reviews_730945.csv
│   │   ├── steam_info_cache.csv
│   │   ├── steam_match_cache.csv
│   │   ├── steam_search_attempt_cache.csv
│   │   └── vgchartz_raw.csv
│   ├── processed/
│   │   ├── key_variable_overview.csv
│   │   ├── master_dataset.csv
│   │   └── publisher_overview.csv
│   └── plots/
│       ├── 02_data_curation_overview/
│       ├── 04_pca/
│       ├── 05_clustering/
│       ├── 06_regression/
│       ├── 07_cart/
│       └── sentiment_analysis_output/
│           ├── controversial_game_wordcloud.png
│           ├── correlation_avgsentiment_vs_sales.png
│           ├── correlation_posratio_vs_sales.png
│           ├── correlation_results.csv
│           ├── correlation_reviews_vs_sales.png
│           ├── correlation_sd_vs_sales.png
│           ├── game_sentiment_summary_full.csv
│           └── merged_sentiment_sales.csv
│
├── scripts/
│   ├── 01_data_scraping.R          # Collect raw data from VGChartz, Steam API, and Steam Reviews - Jacob
│   ├── 02_data_curation.R          # Join, clean, encode variables; plot overviews to justify decisions - Jacob
│   ├── 03_sentiment_analysis.R     # Score review text and append sentiment features to master dataset - William
│   ├── 04_pca.R                    # Reduce correlated predictors; visualise predictor structure - Mike
│   ├── 05_clustering.R             # Segment games into market tiers; append cluster as a predictor - Mike
│   ├── 06_regression.R             # Predict sales volume (linear) and commercial success (logistic) - Daniel
│   └── 07_cart.R                   # Validate regression findings; rank pre-launch attributes by importance - Min
│
└── README.md
```

## Data Sources

This project uses a multi-source pipeline combining online game-sales sources, Steam platform metadata endpoints, and local review snapshots.

### 1. Console-inclusive game sales (online)

- vgsales_andvise
	- https://raw.githubusercontent.com/andvise/DataAnalyticsDatasets/main/vgsales.csv
	- Used fields (standardized): game title, platform, release year, genre, publisher, regional sales, global sales.
- vgsales_saemaqazi
	- https://raw.githubusercontent.com/saemaqazi/vgsales.csv/main/vgsales.csv
	- Used fields (standardized): game title, platform, release year, genre, publisher, regional sales, global sales.
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

Note: this pipeline does not pull full review text from the Steam API in [scripts/01_data_scraping.R](scripts/01_data_scraping.R). Sentiment analysis in [scripts/03_sentiment_analysis.R](scripts/03_sentiment_analysis.R) uses the local review snapshot [data/raw/steam_game_reviews_730945.csv](data/raw/steam_game_reviews_730945.csv).

### 3. Raw data files produced or maintained in this repository

- [data/raw/console_sales_online.csv](data/raw/console_sales_online.csv)
	- Combined and standardized online console-inclusive sales records.
- [data/raw/vgchartz_raw.csv](data/raw/vgchartz_raw.csv)
	- Standardized sales base file used by curation.
- [data/raw/publisher_market_sales_online.csv](data/raw/publisher_market_sales_online.csv)
	- Publisher-year sales totals, game counts, and computed market share metrics.
- [data/raw/publisher_market_share_template.csv](data/raw/publisher_market_share_template.csv)
	- Publisher-year market share table used in joins during curation.
- [data/raw/steam_api_raw.rds](data/raw/steam_api_raw.rds)
	- Steam metadata snapshot from the Steam endpoints.
- [data/raw/steam_game_reviews_730945.csv](data/raw/steam_game_reviews_730945.csv)
	- Local Steam review text snapshot used for sentiment scoring.
- [data/raw/steam_info_cache.csv](data/raw/steam_info_cache.csv)
	- Cache of app-level Steam info responses.
- [data/raw/steam_match_cache.csv](data/raw/steam_match_cache.csv)
	- Cache of title-to-appid matching results.
- [data/raw/steam_search_attempt_cache.csv](data/raw/steam_search_attempt_cache.csv)
	- Cache of search attempts and fallback queries during app matching.


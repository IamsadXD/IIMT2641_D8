# Predictive Analysis of Video Game Commercial Sales Performance: A Data-Driven Study Using Regression and Classification Models

## Project Structure

```
project_root/
│
├── data/
│   ├── raw/
│   │   ├── vgchartz_raw.csv        # Raw game sales figures from VGChartz
│   │   ├── steam_api_raw.rds       # Game attributes from Steam Store API
│   │   ├── steam_reviews_raw.rds   # Raw review text for sentiment analysis
│   │   └── kaggle_vgsales.csv      # Static supplement for pre-2017 titles
│   └── processed/
│       ├── master_dataset.csv      # Final joined and encoded analysis-ready dataset
│       └── sentiment_scores.csv    # Per-game aggregated sentiment metrics
│
├── scripts/
│   ├── 01_data_scraping.R          # Collect raw data from VGChartz, Steam API, and Steam Reviews
│   ├── 02_data_curation.R          # Join, clean, encode variables; plot overviews to justify decisions
│   ├── 03_sentiment_analysis.R     # Score review text and append sentiment features to master dataset
│   ├── 04_pca.R                    # Reduce correlated predictors; visualise predictor structure
│   ├── 05_clustering.R             # Segment games into market tiers; append cluster as a predictor
│   ├── 06_regression.R             # Predict sales volume (linear) and commercial success (logistic)
│   └── 07_cart.R                   # Validate regression findings; rank pre-launch attributes by importance
│
└── README.md
```

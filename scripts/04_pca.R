# ==============================================
# Video Game Sales Analysis: PCA for Predictor Reduction
# Purpose: Reduce correlated pre-launch game attributes
# Author: Mike
# ==============================================

# Load required libraries (run once at the start)
# Install packages ONLY ONCE if not already installed
# install.packages(c("tidyverse", "factoextra", "corrplot"))
library(tidyverse)
library(factoextra)
library(corrplot)

plots_dir <- "data/plots/04_pca"

dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
dir.create("data/results", recursive = TRUE, showWarnings = FALSE)

# ==============================================
# Step 1: Load All Raw Datasets
# ==============================================
cat("=== LOADING RAW DATASETS ===\n")

# Core game sales data from VGChartz
vgchartz <- read.csv("data/raw/vgchartz_raw.csv", stringsAsFactors = FALSE)
cat("✓ VGChartz: Rows =", nrow(vgchartz), "| Columns =", ncol(vgchartz), "\n")

# Publisher annual sales metrics
publisher_sales <- read.csv("data/raw/publisher_market_sales_online.csv", stringsAsFactors = FALSE)
cat("✓ Publisher Sales: Rows =", nrow(publisher_sales), "| Columns =", ncol(publisher_sales), "\n")

# Console/platform sales data
console_sales <- read.csv("data/raw/console_sales_online.csv", stringsAsFactors = FALSE)
cat("✓ Console Sales: Rows =", nrow(console_sales), "| Columns =", ncol(console_sales), "\n")

# Publisher market share data
publisher_share <- read.csv("data/processed/publisher_market_share_derived.csv", stringsAsFactors = FALSE)
cat("✓ Publisher Share: Rows =", nrow(publisher_share), "| Columns =", ncol(publisher_share), "\n")

# ==============================================
# Step 2: Inspect Column Names to Identify Merge Keys
# ==============================================
cat("\n=== CHECKING COLUMN NAMES FOR DATA MERGING ===\n")
cat("--- VGChartz Columns ---\n"); print(colnames(vgchartz))
cat("--- Publisher Sales Columns ---\n"); print(colnames(publisher_sales))
cat("--- Console Sales Columns ---\n"); print(colnames(console_sales))
cat("--- Publisher Share Columns ---\n"); print(colnames(publisher_share))

# ==============================================
# Step 3: Merge Datasets (Game-Level + Publisher-Level)
# ==============================================
cat("\n=== MERGING GAME-LEVEL DATA (VGChartz + Console Sales) ===\n")
# Merge using unique game identifiers: title, publisher, platform, release year
game_level <- vgchartz %>%
  inner_join(console_sales,
             by = c("game_title", "publisher", "platform", "release_year"),
             suffix = c("", "_console"))

cat("\n=== MERGING PUBLISHER-LEVEL DATA (Sales + Market Share) ===\n")
# Merge using publisher and year
publisher_level <- publisher_sales %>%
  inner_join(publisher_share,
             by = c("publisher", "year"),
             suffix = c("", "_share"))

cat("\n=== FINAL MERGE: GAME DATA + PUBLISHER DATA ===\n")
# Combine all data into one master dataset
final_data <- game_level %>%
  mutate(year = release_year) %>%
  left_join(publisher_level, by = c("publisher", "year")) %>%
  filter(!is.na(game_title))

# ==============================================
# Step 4: Clean Data for PCA & Clustering
# ==============================================
cat("\n=== SELECTING NUMERIC FEATURES FOR PCA ===\n")
# Keep game identifiers, target variable (global sales), and pre-launch numeric predictors
analysis_data <- final_data %>%
  select(
    # Game identifiers
    game_title, publisher, platform, release_year,
    # Target variable: sales performance
    global_sales,
    # Pre-launch numeric predictors (PCA input)
    year, market_share_pct, publisher_global_sales, game_count, avg_sales_per_game
  ) %>%
  # Remove rows with missing values (required for PCA)
  drop_na(global_sales, market_share_pct, publisher_global_sales)

# Save cleaned dataset for downstream analysis
write.csv(analysis_data, "data/results/clean_for_pca_clustering.csv", row.names = FALSE)
cat("\n✅ Cleaned data saved: data/results/clean_for_pca_clustering.csv\n")
cat("Final Clean Dataset Rows:", nrow(analysis_data), "\n")

# ==============================================
# Step 5: Principal Components Analysis (PCA)
# Integrated with lecture-based PCA methodology
# Purpose: Reduce correlated predictors & analyze attribute importance
# ==============================================
cat("\n=== RUNNING PRINCIPAL COMPONENTS ANALYSIS ===\n")

# Load cleaned data
data <- read.csv("data/results/clean_for_pca_clustering.csv")

# Select pre-launch numeric features for PCA
pca_features <- data %>%
  select(year, market_share_pct, publisher_global_sales, game_count, avg_sales_per_game)

# Run PCA with standardization (lecture standard: scale. = TRUE)
pr.out <- prcomp(pca_features, scale. = TRUE)

# ==============================================
# Step 6: PCA Diagnostic Output (For Academic Report)
# ==============================================
cat("\n=== PCA MEANS & STANDARD DEVIATIONS (Standardization Info) ===\n")
pr.out$center  # Mean of original variables
pr.out$scale   # Standard deviation of original variables

cat("\n=== PCA ROTATION MATRIX (Feature Loadings) ===\n")
# Loadings: Larger absolute value = stronger contribution to the component
print(pr.out$rotation)

cat("\n=== PCA COMPONENT SCORES (First 6 Rows) ===\n")
pca_components <- pr.out$x
print(head(pca_components))

# Verify PCA success: Components must be uncorrelated (values ~ 0)
cat("\n=== PCA COMPONENT CORRELATION MATRIX ===\n")
print(cor(pca_components))

# Variance explanation (core statistical output for report)
cat("\n=== PCA VARIANCE EXPLAINED SUMMARY ===\n")
pca_summary <- summary(pr.out)
print(pca_summary)
importance <- pca_summary$importance

# ==============================================
# Step 7: PCA Visualizations (Report-Ready Plots)
# ==============================================
# Plot 1: Proportion of Variance + Cumulative Variance
png(file.path(plots_dir, "pca_variance_explained.png"), width = 1600, height = 700, res = 140)
par(mfrow = c(1, 2))
plot(importance[2,], 
     xlab = "Principal Component",
     ylab = "Proportion of Variance Explained", 
     ylim = c(0, 1), type = "b", col = "red", lwd = 2)
plot(importance[3,], 
     xlab = "Principal Component",
     ylab = "Cumulative Variance Explained",
     ylim = c(0, 1), type = "b", col = "blue", lwd = 2)
par(mfrow = c(1, 1))
dev.off()

# Plot 2: PCA Biplot (Optimized for large datasets)
pr.out$rotation <- -pr.out$rotation
pr.out$x <- -pr.out$x
par(mfrow = c(1, 1))  # Reset layout to 1 plot per page

# Use all available games in the PCA biplot
biplot_obj <- fviz_pca_biplot(pr.out,
                      repel = TRUE,
                      col.var = "red",
                      col.ind = "steelblue",
                      geom.ind = "point",  # Only show points, no text labels for games
                      pointsize = 0.5     # Make game points smaller
) +
  labs(title = "PCA Biplot: Pre-Launch Game Attributes (All Games)") +
  theme_minimal()

print(biplot_obj)
ggsave(file.path(plots_dir, "pca_biplot_sampled.png"), plot = biplot_obj, width = 11, height = 7, dpi = 300)



# ==============================================
# Step 8: Save PCA Results for Clustering
# ==============================================
final_pca_data <- cbind(data, pr.out$x)
write.csv(final_pca_data, "data/results/data_with_pca.csv", row.names = FALSE)

cat("\n✅ PCA ANALYSIS COMPLETED SUCCESSFULLY!")
cat("\n✅ Output File Saved: data/results/data_with_pca.csv\n")


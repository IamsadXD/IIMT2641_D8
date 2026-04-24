# ==============================================
# Video Game Sales Analysis - K-Means Clustering
# Course: IIMT 2601 (HKU)
# Purpose: Segment games into market tiers using PCA results
# Method: K-means Clustering (aligned with school lecture)
# ==============================================

# ---------------------------
# 1. Setup Working Directory & Libraries
# ---------------------------
# Set to your project folder (SAME as PCA)
setwd("C:/Users/Mikek/Desktop/HKU University/IIMT 2601 Codes for R/Group Project")
getwd()

# Install packages (RUN ONCE if missing)
# install.packages(c("tidyverse", "factoextra", "cluster"))

# Load core libraries (school standard)
library(tidyverse)
library(factoextra)  # For Elbow Plot & clustering visualization
library(cluster)     # For K-means validation

# ---------------------------
# 2. Load PCA Processed Data (Output from PCA Step)
# ---------------------------
cat("=== LOADING PCA DATA FOR CLUSTERING ===\n")
game_data <- read.csv("data_with_pca.csv")

# Check data structure
cat("Dataset Dimensions:", dim(game_data), "\n")
cat("Key Columns (PCA Components + Sales):\n")
colnames(game_data)

# ---------------------------
# 3. Prepare Clustering Data
# Use PC1 + PC2 (explains 77.49% variance - OPTIMAL for clustering)
# ---------------------------
cluster_input <- game_data %>%
  select(PC1, PC2)  # Use top 2 principal components (school best practice)

cat("=== Clustering using PC1 & PC2 (77.49% variance explained) ===\n")

# ---------------------------
# 4. School Lecture Key: Elbow Method (Find OPTIMAL Number of Clusters) apprantly this crushs the R studio
# ---------------------------
#cat("\n=== RUNNING ELBOW METHOD TO DETERMINE BEST K ===\n")
# Elbow plot = standard method from your class to select K
#fviz_nbclust(cluster_input, kmeans, method = "wss") +
 # labs(title = "Elbow Method: Optimal Number of Clusters",
  #     x = "Number of Clusters (K)",
   #    y = "Total Within Sum of Squares") +
#  theme_minimal()

# ---------------------------
# 5. K-Means Clustering (K=3: Industry Standard: Blockbuster / Mid-Tier / Indie)
# Set seed = REPRODUCIBLE (required in your school assignments)
# ---------------------------
set.seed(123)  # Fixed random seed (school requirement for consistent results)
k <- 3  # Optimal K selected from Elbow Plot + industry logic
kmeans_model <- kmeans(cluster_input, centers = k, nstart = 25)

# ---------------------------
# 6. School Lecture Key: Cluster Frequency Statistics
# ---------------------------
cat("\n=== CLUSTER SIZE DISTRIBUTION ===\n")
cluster_counts <- table(kmeans_model$cluster)
print(cluster_counts)

# ---------------------------
# 7. Assign Cluster Labels & Name Market Tiers
# ---------------------------
game_data_final <- game_data %>%
  mutate(
    cluster = kmeans_model$cluster,
    # Name clusters based on PCA findings (Publisher Strength = PC1)
    market_tier = case_when(
      cluster == 1 ~ "Blockbuster",   # High publisher power
      cluster == 2 ~ "Mid-Tier",       # Medium publisher power
      cluster == 3 ~ "Indie"           # Low publisher power
    )
  )

# ---------------------------
# 8. School Lecture Key: Cluster Feature Analysis (Mean Comparison)
# Calculate mean values of ORIGINAL pre-launch variables per cluster
# This is the critical analysis from your class (tapply logic)
# ---------------------------
cat("\n=== CLUSTER PROFILING (Mean Values of Pre-Launch Attributes) ===\n")
cluster_profile <- game_data_final %>%
  group_by(market_tier) %>%
  summarise(
    count = n(),
    avg_year = mean(year, na.rm = TRUE),
    avg_market_share = mean(market_share_pct, na.rm = TRUE),
    avg_publisher_sales = mean(publisher_global_sales, na.rm = TRUE),
    avg_game_count = mean(game_count, na.rm = TRUE),
    avg_sales_per_game = mean(avg_sales_per_game, na.rm = TRUE),
    avg_global_sales = mean(global_sales, na.rm = TRUE)
  ) %>%
  arrange(desc(avg_publisher_sales))

print(cluster_profile)

# ---------------------------
# 9. Clustering Visualization (Simple, Report-Ready)
# ---------------------------
fviz_cluster(kmeans_model, data = cluster_input,
             geom = "point",
             pointsize = 0.8,
             palette = c("#E74C3C", "#3498DB", "#2ECC71"),
             ellipse.type = "convex") +
  labs(title = "Game Market Segmentation (K-means Clustering)",
       subtitle = "Based on PCA Pre-Launch Attributes",
       color = "Market Tier") +
  theme_minimal()

# ---------------------------
# 10. Save Final Clustered Data (For Team Regression/Classification)
# ---------------------------
write.csv(game_data_final, "final_game_data_with_clusters.csv", row.names = FALSE)

# ---------------------------
# 11. Final Output Summary
# ---------------------------
cat("\n✅ CLUSTERING ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("✅ Final File Saved: final_game_data_with_clusters.csv\n")
cat("✅ Market Tiers Created: Blockbuster, Mid-Tier, Indie\n")
cat("✅ Clusters based on PC1 (Publisher Strength) & PC2 (Year/Scale)\n")


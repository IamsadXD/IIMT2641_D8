# ============================================
# COMPLETE STEAM GAME SENTIMENT ANALYSIS
# Using Cleaned Reviews + AFINN (WITH NORMALIZATION)
# ============================================

library(syuzhet)
library(dplyr)
library(ggplot2)

# ============================================
# Part 1: Load Data
# ============================================

cat("=== LOADING DATA ===\n")
steam_data <- read.csv("/Users/user/Downloads/steam_game_reviews_730945.csv") #change if needed
cat("Total reviews:", nrow(steam_data), "\n")
cat("Total unique games:", length(unique(steam_data$name)), "\n")

# ============================================
# Part 2: Function to Clean Reviews
# ============================================

clean_review <- function(text) {
  # Convert to character
  text <- as.character(text)
  
  # Remove line breaks, carriage returns, tabs
  text <- gsub("[\r\n\t]", " ", text)
  
  # Remove URLs
  text <- gsub("http\\S+|www\\S+", "", text)
  
  # Remove HTML tags
  text <- gsub("<.*?>", "", text)
  
  # Remove non-ASCII characters
  text <- iconv(text, "UTF-8", "ASCII", sub = " ")
  
  # Keep only letters and spaces (remove punctuation except apostrophes)
  text <- gsub("[^a-zA-Z\\s']", " ", text)
  
  # Convert to lowercase
  text <- tolower(text)
  
  # Remove extra spaces
  text <- gsub("\\s+", " ", text)
  
  # Trim leading/trailing spaces
  text <- trimws(text)
  
  return(text)
}

# ============================================
# Part 3: Clean ALL Reviews (This will take time)
# ============================================

cat("\n=== CLEANING REVIEWS ===\n")
cat("This will take 5-10 minutes for 730,945 reviews...\n")

# Clean all reviews
steam_data$clean_review <- sapply(steam_data$review, clean_review)

cat("✓ Cleaning complete!\n")

# ============================================
# Part 4: Apply AFINN Sentiment WITH NORMALIZATION
# ============================================

cat("\n=== APPLYING AFINN SENTIMENT (WITH NORMALIZATION) ===\n")
cat("Processing all reviews for sentiment scores...\n")

# Get RAW AFINN scores for all cleaned reviews
raw_scores <- get_sentiment(steam_data$clean_review, method = "afinn")

# Count words per review for normalization
steam_data$word_count <- sapply(strsplit(steam_data$clean_review, " "), length)

# NORMALIZE: Score per 10 words, then clamp to -5 to +5
# This ensures reviews of different lengths are comparable
steam_data$sentiment_score <- (raw_scores / (steam_data$word_count / 10))
steam_data$sentiment_score <- pmin(pmax(steam_data$sentiment_score, -5), 5)

# For zero-word reviews (empty), set score to 0
steam_data$sentiment_score[is.na(steam_data$sentiment_score)] <- 0
steam_data$sentiment_score[is.infinite(steam_data$sentiment_score)] <- 0

# Add classification columns
steam_data$is_positive <- steam_data$sentiment_score > 0
steam_data$sentiment_category <- ifelse(
  steam_data$sentiment_score > 0, "Positive",
  ifelse(steam_data$sentiment_score < 0, "Negative", "Neutral")
)

cat("\n=== AFINN NORMALIZED RESULTS ===\n")
cat("Raw score range:", min(raw_scores), "to", max(raw_scores), "\n")
cat("Normalized score range:", min(steam_data$sentiment_score), "to", max(steam_data$sentiment_score), "\n")
cat("Mean normalized score:", mean(steam_data$sentiment_score), "\n")
cat("Positive reviews:", sum(steam_data$sentiment_score > 0), "\n")
cat("Negative reviews:", sum(steam_data$sentiment_score < 0), "\n")
cat("Neutral reviews:", sum(steam_data$sentiment_score == 0), "\n")

# ============================================
# Part 5: Create Game Summary with ALL Metrics
# ============================================

cat("\n=== CREATING GAME SUMMARY ===\n")

game_summary <- steam_data %>%
  group_by(name) %>%
  summarise(
    # Basic counts
    total_reviews = n(),
    positive_count = sum(is_positive, na.rm = TRUE),
    negative_count = sum(sentiment_category == "Negative", na.rm = TRUE),
    neutral_count = sum(sentiment_category == "Neutral", na.rm = TRUE),
    
    # Ratios
    positive_ratio = positive_count / total_reviews * 100,
    negative_ratio = negative_count / total_reviews * 100,
    neutral_ratio = neutral_count / total_reviews * 100,
    
    # Sentiment scores (now properly normalized to -5 to +5)
    avg_sentiment_score = mean(sentiment_score, na.rm = TRUE),
    median_sentiment_score = median(sentiment_score, na.rm = TRUE),
    min_sentiment_score = min(sentiment_score, na.rm = TRUE),
    max_sentiment_score = max(sentiment_score, na.rm = TRUE),
    sd_sentiment_score = sd(sentiment_score, na.rm = TRUE),
    
    # Extreme emotions (scores >= 3 or <= -3, now meaningful)
    very_positive_count = sum(sentiment_score >= 3, na.rm = TRUE),
    very_negative_count = sum(sentiment_score <= -3, na.rm = TRUE),
    
    .groups = "drop"
  ) %>%
  arrange(desc(total_reviews))

# Rename the first column to "game_title"
colnames(game_summary)[1] <- "game_title"

# ============================================
# Part 6: Save the Game Summary File
# ============================================

# Save full summary
write.csv(game_summary, "game_sentiment_summary_full.csv", row.names = FALSE)
cat("\n✓ Saved: game_sentiment_summary_full.csv\n")
cat("  Total games in summary:", nrow(game_summary), "\n")

# ============================================
# Part 7: Verify Normalization Worked
# ============================================

cat("\n=== VERIFY NORMALIZATION ===\n")
cat("avg_sentiment_score range:", 
    round(min(game_summary$avg_sentiment_score, na.rm = TRUE), 2), "to",
    round(max(game_summary$avg_sentiment_score, na.rm = TRUE), 2), "\n")

outside_range <- sum(game_summary$avg_sentiment_score > 5 | game_summary$avg_sentiment_score < -5, na.rm = TRUE)
if(outside_range == 0) {
  cat("✓ SUCCESS: All scores are within -5 to +5 range!\n")
} else {
  cat("⚠️ Warning:", outside_range, "games still outside -5 to +5 range\n")
}

# ============================================
# Part 8: Display Sample Results
# ============================================

cat("\n=== SAMPLE OF GAME SUMMARY (First 15 games) ===\n")
print(head(game_summary[, c("game_title", "total_reviews", "positive_count", 
                            "negative_count", "neutral_count", "positive_ratio", 
                            "negative_ratio", "avg_sentiment_score", 
                            "very_positive_count", "very_negative_count")], 15))

# ============================================
# Part 9: Show Best and Worst Games
# ============================================

cat("\n=== TOP 10 MOST POSITIVE GAMES ===\n")
top_positive <- game_summary %>%
  filter(total_reviews >= 50) %>%
  arrange(desc(positive_ratio)) %>%
  head(10)
print(top_positive[, c("game_title", "positive_ratio", "total_reviews", "avg_sentiment_score")])

cat("\n=== TOP 10 MOST NEGATIVE GAMES ===\n")
top_negative <- game_summary %>%
  filter(total_reviews >= 50) %>%
  arrange(positive_ratio) %>%
  head(10)
print(top_negative[, c("game_title", "positive_ratio", "total_reviews", "avg_sentiment_score")])

# ============================================
# Part 10: Summary Statistics
# ============================================

cat("\n=== SUMMARY STATISTICS ===\n")
cat(sprintf("Total games analyzed: %d\n", nrow(game_summary)))
cat(sprintf("Games with > 50 reviews: %d\n", sum(game_summary$total_reviews >= 50)))
cat(sprintf("Average positive ratio: %.1f%%\n", mean(game_summary$positive_ratio)))
cat(sprintf("Median positive ratio: %.1f%%\n", median(game_summary$positive_ratio)))
cat(sprintf("Average sentiment score: %.2f (normalized to -5 to +5)\n", 
            mean(game_summary$avg_sentiment_score)))
cat(sprintf("Games with > 80%% positive: %d\n", sum(game_summary$positive_ratio > 80)))
cat(sprintf("Games with < 40%% positive: %d\n", sum(game_summary$positive_ratio < 40)))

# ============================================
# Part 11: Create Overlapping Summary
# ============================================

cat("\n=== CHECKING OVERLAP WITH MASTER DATA ===\n")

# Read your master data
master_data <- read.csv("/Users/user/Downloads/iimt2641/Grp/IIMT2641_D8/data/processed/master_dataset.csv")

# Find overlapping games
overlapping_games <- intersect(game_summary$game_title, master_data$game_title)
cat("Overlapping games:", length(overlapping_games), "\n")

if(length(overlapping_games) > 0) {
  overlapping_summary <- game_summary %>%
    filter(game_title %in% overlapping_games)
  
  write.csv(overlapping_summary, "IIMT2641_D8/data/processed/game_sentiment_summary.csv", row.names = FALSE)
  cat("✓ Saved overlapping summary to: IIMT2641_D8/data/processed/game_sentiment_summary.csv\n")
}

# ============================================
# Part 12: Save Detailed Review Data
# ============================================

review_sample <- steam_data[1:1000, c("name", "review", "clean_review", "word_count", 
                                       "sentiment_score", "sentiment_category")]
write.csv(review_sample, "review_sentiment_sample.csv", row.names = FALSE)
cat("✓ Saved review sample to: review_sentiment_sample.csv\n")

# ============================================
# Part 13: Histogram Visualization (Now Within -5 to +5)
# ============================================

# Plot 1: Positive Ratio Distribution (0 to 100)
p1 <- ggplot(game_summary, aes(x = positive_ratio)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(game_summary$positive_ratio, na.rm = TRUE), 
             color = "red", linetype = "dashed", size = 1) +
  labs(title = "Distribution of Positive Review Ratios (All Steam Games)",
       subtitle = paste("Mean:", round(mean(game_summary$positive_ratio, na.rm = TRUE), 1), "%"),
       x = "Positive Review Ratio (%)",
       y = "Number of Games") +
  xlim(0, 100) +
  theme_minimal()
print(p1)

# Plot 2: Average AFINN Score Distribution (Now properly -5 to +5)
p2 <- ggplot(game_summary, aes(x = avg_sentiment_score)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black", alpha = 0.7) +
  geom_vline(xintercept = mean(game_summary$avg_sentiment_score, na.rm = TRUE), 
             color = "red", linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0, color = "gray", linetype = "dotted", size = 0.5) +
  labs(title = "Distribution of Average AFINN Scores (Normalized)",
       subtitle = paste("Mean:", round(mean(game_summary$avg_sentiment_score, na.rm = TRUE), 2),
                        "| Range: -5 to +5 | N =", nrow(game_summary), "games"),
       x = "Average Sentiment Score",
       y = "Number of Games") +
  xlim(-5, 5) +
  theme_minimal()
print(p2)

# Save plots
ggsave("positive_ratio_distribution.png", p1, width = 8, height = 6, dpi = 300)
ggsave("avg_sentiment_normalized.png", p2, width = 8, height = 6, dpi = 300)

cat("\n✓ Plots saved:\n")
cat("  - positive_ratio_distribution.png\n")
cat("  - avg_sentiment_normalized.png\n")

# ============================================
# Part 14: Final Summary
# ============================================

cat("\n")
cat("========================================\n")
cat("========== ANALYSIS COMPLETE! ==========\n")
cat("========================================\n")
cat("\n")
cat("NORMALIZATION APPLIED:\n")
cat("  - Raw AFINN scores divided by (word_count / 10)\n")
cat("  - Scores clamped to -5 to +5 range\n")
cat("  - This ensures fair comparison across reviews of different lengths\n")
cat("\n")
cat("FILES CREATED:\n")
cat("1. game_sentiment_summary_full.csv - ALL", nrow(game_summary), "Steam games\n")
cat("2. IIMT2641_D8/data/processed/game_sentiment_summary.csv - Overlapping games\n")
cat("3. review_sentiment_sample.csv - Sample reviews with scores\n")
cat("\n")
cat("COLUMNS IN game_sentiment_summary_full.csv:\n")
cat("- game_title\n")
cat("- total_reviews\n")
cat("- positive_count / negative_count / neutral_count\n")
cat("- positive_ratio / negative_ratio / neutral_ratio\n")
cat("- avg_sentiment_score (NORMALIZED to -5 to +5)\n")
cat("- median_sentiment_score\n")
cat("- min_sentiment_score / max_sentiment_score\n")
cat("- sd_sentiment_score (standard deviation)\n")
cat("- very_positive_count / very_negative_count (scores >=3 or <=-3)\n")

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
steam_data <- read.csv("data/raw/steam_game_reviews_730945.csv") 
cat("Total raw reviews:", nrow(steam_data), "\n")

# Load master dataset first; use steamid to keep only related appids before cleaning
master_data <- read.csv("data/processed/master_dataset.csv")

if(!"appid" %in% names(steam_data)) {
  stop("Column 'appid' not found in steam review data. Please include appid in input file.")
}

if(!"steamid" %in% names(master_data)) {
  stop("Column 'steamid' not found in master_dataset.csv. Run scripts/02_data_curation.R first.")
}

steam_data <- steam_data %>%
  mutate(appid = as.character(appid))

master_steamids <- master_data %>%
  mutate(steamid = as.character(steamid)) %>%
  filter(!is.na(steamid), steamid != "") %>%
  distinct(steamid)

total_reviews_before_filter <- nrow(steam_data)
unique_appids_before_filter <- n_distinct(steam_data$appid)

steam_data <- steam_data %>%
  filter(appid %in% master_steamids$steamid)

cat("Unique appids in reviews (before filter):", unique_appids_before_filter, "\n")
cat("Unique steamids in master dataset:", nrow(master_steamids), "\n")
cat("Reviews after steamid/appid filter:", nrow(steam_data), "(removed", total_reviews_before_filter - nrow(steam_data), ")\n")
cat("Unique games after filter:", length(unique(steam_data$name)), "\n")

if(nrow(steam_data) == 0) {
  stop("No review rows matched master_dataset steamids. Check appid/steamid format and source files.")
}

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
cat("Cleaning only rows matched to master_dataset steamids...\n")
cat("Reviews to clean:", nrow(steam_data), "\n")

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

# Ensure appid exists before app-level aggregation
if(!"appid" %in% names(steam_data)) {
  stop("Column 'appid' not found in steam review data. Please include appid in input file.")
}

game_summary <- steam_data %>%
  mutate(appid = as.character(appid)) %>%
  group_by(appid) %>%
  summarise(
    game_title = first(name),
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

# ============================================
# Part 6: Save the Game Summary File
# ============================================

# Save full summary
write.csv(game_summary, "data/plots/sentiment_analysis_output/game_sentiment_summary_full.csv", row.names = FALSE)
cat("\n✓ Saved: data/plots/sentiment_analysis_output/game_sentiment_summary_full.csv\n")
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
# Part 11: Create Merged Data for Analysis
# ============================================

cat("\n=== CREATING MERGED DATA FOR ANALYSIS ===\n")

# Keep only the columns you need; join on steamid matched to review appid
master_sales <- master_data %>%
  mutate(steamid = as.character(steamid)) %>%
  filter(!is.na(steamid), steamid != "") %>%
  select(steamid, global_sales) %>%
  distinct(steamid, .keep_all = TRUE) %>%
  rename(appid = steamid)

# Merge with your game summary (from Part 5)
merged_analysis <- game_summary %>%
  inner_join(master_sales, by = "appid")

cat("Merged data size:", nrow(merged_analysis), "games\n")
cat("Games with sales data:", sum(!is.na(merged_analysis$global_sales)), "\n")

# Save merged data for analysis
write.csv(merged_analysis, "data/plots/sentiment_analysis_output/merged_sentiment_sales.csv", row.names = FALSE)
cat("✓ Saved: data/plots/sentiment_analysis_output/merged_sentiment_sales.csv\n")

# ============================================
# PART 12: CORRELATION ANALYSIS (Second Part)
# ============================================

cat("\n")
cat("========================================\n")
cat("=== PART 2: SENTIMENT vs SALES ANALYSIS ===\n")
cat("========================================\n\n")

# Remove rows with missing sales data
analysis_data <- merged_analysis %>%
  filter(!is.na(global_sales), global_sales > 0)

cat("Games in analysis (with valid sales data):", nrow(analysis_data), "\n\n")

# ============================================
# Correlation Analysis
# ============================================

cat("=== CORRELATION MATRIX ===\n\n")

# List of sentiment metrics to correlate with sales
metrics <- c(
  "positive_ratio",
  "negative_ratio", 
  "neutral_ratio",
  "avg_sentiment_score",
  "median_sentiment_score",
  "sd_sentiment_score",
  "very_positive_count",
  "very_negative_count",
  "total_reviews"
)

# Calculate correlations
correlation_results <- data.frame(
  Metric = character(),
  Correlation = numeric(),
  P_value = numeric(),
  Significant = character(),
  stringsAsFactors = FALSE
)

for(metric in metrics) {
  # Remove NAs
  valid_data <- analysis_data[!is.na(analysis_data[[metric]]), ]
  
  if(nrow(valid_data) > 5) {
    cor_test <- cor.test(valid_data[[metric]], valid_data$global_sales, method = "pearson")
    
    correlation_results <- rbind(correlation_results, data.frame(
      Metric = metric,
      Correlation = round(cor_test$estimate, 4),
      P_value = round(cor_test$p.value, 6),
      Significant = ifelse(cor_test$p.value < 0.05, "Yes", "No"),
      stringsAsFactors = FALSE
    ))
  }
}

# Sort by absolute correlation
correlation_results <- correlation_results %>%
  arrange(desc(abs(Correlation)))

print(correlation_results)

# ============================================
# Key Findings Summary
# ============================================

cat("\n=== KEY FINDINGS ===\n")

# Find best positive correlation
best_positive <- correlation_results %>%
  filter(Correlation > 0) %>%
  arrange(desc(Correlation)) %>%
  head(1)

# Find best negative correlation
best_negative <- correlation_results %>%
  filter(Correlation < 0) %>%
  arrange(Correlation) %>%
  head(1)

if(nrow(best_positive) > 0) {
  cat(sprintf("\n✓ Strongest POSITIVE correlation: %s (r = %.3f, p = %.6f)\n", 
              best_positive$Metric[1], best_positive$Correlation[1], best_positive$P_value[1]))
}

if(nrow(best_negative) > 0) {
  cat(sprintf("✓ Strongest NEGATIVE correlation: %s (r = %.3f, p = %.6f)\n", 
              best_negative$Metric[1], best_negative$Correlation[1], best_negative$P_value[1]))
}

# Significance summary
sig_count <- sum(correlation_results$Significant == "Yes")
cat(sprintf("\n✓ %d out of %d metrics show statistically significant correlation (p < 0.05)\n", 
            sig_count, nrow(correlation_results)))

# ============================================
# Visualizations
# ============================================

cat("\n=== GENERATING PLOTS ===\n")

# Create directory for plots
dir.create("data/plots/sentiment_analysis_output", recursive = TRUE, showWarnings = FALSE)

# Plot 1: Positive Ratio vs Sales
p3 <- ggplot(analysis_data, aes(x = positive_ratio, y = global_sales)) +
  geom_point(alpha = 0.6, color = "steelblue", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Positive Review Ratio vs Global Sales",
       x = "Positive Review Ratio (%)",
       y = "Global Sales (millions)") +
  theme_minimal()
print(p3)
ggsave("data/plots/sentiment_analysis_output/correlation_posratio_vs_sales.png", p3, width = 8, height = 6, dpi = 300)

# Plot 2: Average Sentiment Score vs Sales
p4 <- ggplot(analysis_data, aes(x = avg_sentiment_score, y = global_sales)) +
  geom_point(alpha = 0.6, color = "darkgreen", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Average Sentiment Score vs Global Sales",
       x = "Average Sentiment Score (normalized -5 to +5)",
       y = "Global Sales (millions)") +
  theme_minimal()
print(p4)
ggsave("data/plots/sentiment_analysis_output/correlation_avgsentiment_vs_sales.png", p4, width = 8, height = 6, dpi = 300)

# Plot 3: Sentiment SD (Controversy) vs Sales
p5 <- ggplot(analysis_data, aes(x = sd_sentiment_score, y = global_sales)) +
  geom_point(alpha = 0.6, color = "purple", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Sentiment Controversy (SD) vs Global Sales",
       x = "Standard Deviation of Sentiment Scores (more controversy = higher SD)",
       y = "Global Sales (millions)") +
  theme_minimal()
print(p5)
ggsave("data/plots/sentiment_analysis_output/correlation_sd_vs_sales.png", p5, width = 8, height = 6, dpi = 300)

# Plot 4: Total Reviews vs Sales
p6 <- ggplot(analysis_data, aes(x = total_reviews, y = global_sales)) +
  geom_point(alpha = 0.6, color = "orange", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "red") +
  labs(title = "Total Reviews vs Global Sales",
       x = "Number of Reviews",
       y = "Global Sales (millions)") +
  theme_minimal()
print(p6)
ggsave("data/plots/sentiment_analysis_output/correlation_reviews_vs_sales.png", p6, width = 8, height = 6, dpi = 300)

# Save Correlation Results
write.csv(correlation_results, "data/plots/sentiment_analysis_output/correlation_results.csv", row.names = FALSE)
cat("\n✓ Saved: correlation_results.csv\n")

# ============================================
# PART 13: WORD CLOUD FOR MOST CONTROVERSIAL GAME
# Based on highest sd_sentiment_score (from correlation findings)
# ============================================

cat("\n")
cat("========================================\n")
cat("=== PART 3: WORD CLOUD FOR CONTROVERSIAL GAME ===\n")
cat("========================================\n\n")

library(tm)
library(wordcloud)
library(RColorBrewer)

# Step 1: Find the most controversial game from game_summary
controversial_game <- game_summary %>%
  filter(total_reviews >= 100) %>%  # At least 100 reviews for meaningful word cloud
  arrange(desc(sd_sentiment_score)) %>%
  head(1)

cat("Most Controversial Game (Highest SD = Most Divided Opinions):\n")
cat("  Game title:", controversial_game$game_title, "\n")
cat("  Positive Ratio:", round(controversial_game$positive_ratio, 1), "%\n")
cat("  Sentiment SD:", round(controversial_game$sd_sentiment_score, 3), "(higher = more divided)\n")
cat("  Total reviews:", controversial_game$total_reviews, "\n\n")

# Step 2: Extract reviews for this game from steam_data
game_reviews <- steam_data %>%
  filter(name == controversial_game$game_title)

cat("Reviews found for this game:", nrow(game_reviews), "\n")

# Step 3: Split into very positive (score >= 3) and very negative (score <= -3)
very_positive <- game_reviews %>%
  filter(sentiment_score >= 3) %>%
  pull(clean_review)

very_negative <- game_reviews %>%
  filter(sentiment_score <= -3) %>%
  pull(clean_review)

cat("Very positive reviews (score >= 4):", length(very_positive), "\n")
cat("Very negative reviews (score <= -4):", length(very_negative), "\n")

# Step 4: Function to create word cloud (Teacher's method)
create_wordcloud_teacher <- function(text_vector, title_name, min_freq = 2) {
  
  if(length(text_vector) < 3) {
    cat("Not enough reviews for:", title_name, "\n")
    return(NULL)
  }
  
  # Combine all text
  combined_text <- paste(text_vector, collapse = " ")
  
  # Create corpus
  TextDoc <- Corpus(VectorSource(combined_text))
  
  # Teacher's preprocessing steps
  toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
  TextDoc <- tm_map(TextDoc, toSpace, "/")
  TextDoc <- tm_map(TextDoc, toSpace, "@")
  TextDoc <- tm_map(TextDoc, toSpace, "\\|")
  TextDoc <- tm_map(TextDoc, tolower)
  TextDoc <- tm_map(TextDoc, removeNumbers)
  TextDoc <- tm_map(TextDoc, removeWords, stopwords("english"))
  TextDoc <- tm_map(TextDoc, removeWords, c("game", "also", "time", "cant", "will", "can", "play", "steam", "like", "just", "get", "one", "even", "go", "would", "could", "actually", "really"))
  TextDoc <- tm_map(TextDoc, removePunctuation)
  TextDoc <- tm_map(TextDoc, stripWhitespace)
  TextDoc <- tm_map(TextDoc, stemDocument)
  
  # Build term-document matrix
  TextDoc_dtm <- TermDocumentMatrix(TextDoc)
  dtm_m <- as.matrix(TextDoc_dtm)
  dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
  dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
  
  if(nrow(dtm_d) == 0) {
    cat("No words found for:", title_name, "\n")
    return(NULL)
  }
  
  # Generate word cloud
  set.seed(1234)
  wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = min_freq,
            max.words = 100, random.order = FALSE, rot.per = 0.40,
            colors = brewer.pal(8, "Dark2"))
  title(main = title_name, cex.main = 0.9)
  
  return(dtm_d)
}

# Step 5: Create and save side-by-side word clouds
# Set up 1x2 plotting area
png(filename = "data/plots/sentiment_analysis_output/controversial_game_wordcloud.png", 
    width = 12, height = 6, units = "in", res = 300)

par(mfrow = c(1, 2), mar = c(1, 1, 3, 1))

# Very Positive word cloud
if(length(very_positive) >= 3) {
  create_wordcloud_teacher(very_positive, 
                           paste(controversial_game$game_title, "\nVery Positive (score >= 3)"),
                           min_freq = 2)
} else {
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, "Not enough very positive reviews", cex = 1.2, col = "red")
}

# Very Negative word cloud
if(length(very_negative) >= 3) {
  create_wordcloud_teacher(very_negative, 
                           paste(controversial_game$game_title, "\nVery Negative (score <= -3)"),
                           min_freq = 2)
} else {
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, "Not enough very negative reviews", cex = 1.2, col = "red")
}

dev.off()
par(mfrow = c(1, 1))

cat("\n✓ Word cloud saved: data/plots/sentiment_analysis_output/controversial_game_wordcloud.png\n")

# Step 6: Print example reviews
cat("\n=== EXAMPLE REVIEWS FROM", controversial_game$game_title, "===\n\n")

cat("--- VERY POSITIVE (score >= 3) ---\n")
positive_examples <- game_reviews %>%
  filter(sentiment_score >= 3) %>%
  head(3)

for(i in 1:nrow(positive_examples)) {
  cat(sprintf("Score: %.1f | %s\n", 
              positive_examples$sentiment_score[i], 
              substr(positive_examples$review[i], 1, 100)))
  cat("---\n")
}

cat("\n--- VERY NEGATIVE (score <= -3) ---\n")
negative_examples <- game_reviews %>%
  filter(sentiment_score <= -3) %>%
  head(3)

for(i in 1:nrow(negative_examples)) {
  cat(sprintf("Score: %.1f | %s\n", 
              negative_examples$sentiment_score[i], 
              substr(negative_examples$review[i], 1, 100)))
  cat("---\n")
}

# ============================================
# FINAL SUMMARY
# ============================================

cat("\n")
cat("========================================\n")
cat("========== ALL ANALYSIS COMPLETE! ==========\n")
cat("========================================\n")
cat("\n")
cat("FILES CREATED:\n")
cat("\n--- Part 1 (Sentiment Analysis) ---\n")
cat("1. data/plots/sentiment_analysis_output/game_sentiment_summary_full.csv\n")
cat("2. data/plots/sentiment_analysis_output/review_sentiment_sample.csv\n")
cat("3. data/plots/sentiment_analysis_output/positive_ratio_distribution.png\n")
cat("4. data/plots/sentiment_analysis_output/avg_sentiment_normalized.png\n")
cat("\n--- Part 2 (Correlation Analysis) ---\n")
cat("5. data/plots/sentiment_analysis_output/merged_sentiment_sales.csv\n")
cat("6. data/plots/sentiment_analysis_output/correlation_results.csv\n")
cat("7. data/plots/sentiment_analysis_output/correlation_posratio_vs_sales.png\n")
cat("8. data/plots/sentiment_analysis_output/correlation_avgsentiment_vs_sales.png\n")
cat("9. data/plots/sentiment_analysis_output/correlation_sd_vs_sales.png\n")
cat("10. data/plots/sentiment_analysis_output/correlation_reviews_vs_sales.png\n")
cat("\n--- Part 3 (Word Cloud for Controversial Game) ---\n")
cat("11. data/plots/sentiment_analysis_output/controversial_game_wordcloud.png\n")
cat("\n")
cat("KEY INSIGHT: If sd_sentiment_score (controversy) correlates with sales,\n")
cat("             the word cloud shows what fans vs critics say about the game.\n")

# === LOADING DATA ===
# Total reviews: 730945 
# Total unique games: 735 
# 
# === CLEANING REVIEWS ===
# This will take 5-10 minutes for 730,945 reviews...
# ✓ Cleaning complete!
# 
# === APPLYING AFINN SENTIMENT (WITH NORMALIZATION) ===
# Processing all reviews for sentiment scores...
# 
# === AFINN NORMALIZED RESULTS ===
# Raw score range: -82 to 134 
# Normalized score range: -5 to 5 
# Mean normalized score: 1.178077 
# Positive reviews: 490209 
# Negative reviews: 141468 
# Neutral reviews: 99268 
# 
# === CREATING GAME SUMMARY ===
# 
# ✓ Saved: data/plots/sentiment_analysis_output/game_sentiment_summary_full.csv
#   Total games in summary: 735 
# 
# === VERIFY NORMALIZATION ===
# avg_sentiment_score range: -0.09 to 2.56 
# ✓ SUCCESS: All scores are within -5 to +5 range!
# 
# === SAMPLE OF GAME SUMMARY (First 15 games) ===
# # A tibble: 15 × 10
#    game_title          total_reviews positive_count negative_count neutral_count
#    <chr>                       <int>          <int>          <int>         <int>
#  1 Call of Duty: Blac…          2000           1277            361           362
#  2 Call of Duty: Blac…          2000           1271            426           303
#  3 Call of Duty: Mode…          2000           1196            478           326
#  4 Call of Duty: Mode…          2000           1319            411           270
#  5 Total War: SHOGUN 2          2000           1330            382           288
#  6 Counter-Strike: Co…          1941           1195            427           319
#  7 Call of Duty: Ghos…          1538            952            399           187
#  8 Medal of Honor               1028            720            187           121
#  9 7 Days to Die                1000            648            242           110
# 10 A Dance of Fire an…          1000            687            166           147
# 11 A Plague Tale: Inn…          1000            771            151            78
# 12 A Story About My U…          1000            781            112           107
# 13 A Way Out                    1000            677            182           141
# 14 ACE COMBAT 7: SKIE…          1000            682            191           127
# 15 AMID EVIL                    1000            696            201           103
# # ℹ 5 more variables: positive_ratio <dbl>, negative_ratio <dbl>,
# #   avg_sentiment_score <dbl>, very_positive_count <int>,
# #   very_negative_count <int>
# 
# === TOP 10 MOST POSITIVE GAMES ===
# # A tibble: 10 × 4
#    game_title                   positive_ratio total_reviews avg_sentiment_score
#    <chr>                                 <dbl>         <int>               <dbl>
#  1 Glass Masquerade                       89.6          1000                2.08
#  2 Kathy Rain                             88.4          1000                1.74
#  3 The Past Within                        87.2          1000                2.27
#  4 Ori and the Will of the Wis…           85.7          1000                2.04
#  5 Unheard - Voices of Crime              85.5          1000                1.64
#  6 Wobbly Life                            85.4          1000                2.56
#  7 Bastion                                85.2          1000                1.86
#  8 Trine Enchanted Edition                85            1000                2.10
#  9 Strobophagia | Rave Horror             85              80                1.23
# 10 GRIS                                   84.8          1000                1.92
# 
# === TOP 10 MOST NEGATIVE GAMES ===
# # A tibble: 10 × 4
#    game_title                   positive_ratio total_reviews avg_sentiment_score
#    <chr>                                 <dbl>         <int>               <dbl>
#  1 EA SPORTS FC 25                        37            1000              0.0296
#  2 Call of Duty: Modern Warfar…           40.8          1000              0.135 
#  3 UNO                                    42.2          1000              0.0233
#  4 EA SPORTS FC 24                        42.5          1000              0.239 
#  5 BattleBit Remastered                   43            1000             -0.0901
#  6 Borderlands: The Pre-Sequel            43            1000              0.0713
#  7 Lobotomy Corporation | Mons…           43.3          1000              0.0402
#  8 Wolfenstein: Youngblood                43.5          1000             -0.0272
#  9 Tom Clancy's Rainbow Six Si…           43.8          1000              0.223 
# 10 Squad                                  44.1          1000              0.100 
# 
# === SUMMARY STATISTICS ===
# Total games analyzed: 735
# Games with > 50 reviews: 731
# Average positive ratio: 67.2%
# Median positive ratio: 67.7%
# Average sentiment score: 1.18 (normalized to -5 to +5)
# Games with > 80% positive: 45
# Games with < 40% positive: 1
# 
# === CREATING MERGED DATA FOR ANALYSIS ===
# Merged data size: 437 games
# Games with sales data: 184 
# ✓ Saved: data/plots/sentiment_analysis_output/merged_sentiment_sales.csv
# 
# ========================================
# === PART 2: SENTIMENT vs SALES ANALYSIS ===
# ========================================
# 
# Games in analysis (with valid sales data): 179 
# 
# === CORRELATION MATRIX ===
# 
#                      Metric Correlation  P_value Significant
# cor8          total_reviews      0.4762 0.000000         Yes
# cor6    very_positive_count      0.3267 0.000008         Yes
# cor7    very_negative_count      0.2999 0.000045         Yes
# cor5     sd_sentiment_score      0.2438 0.001006         Yes
# cor2          neutral_ratio      0.1482 0.047661         Yes
# cor          positive_ratio     -0.0987 0.188791          No
# cor1         negative_ratio      0.0429 0.568520          No
# cor3    avg_sentiment_score     -0.0033 0.964518          No
# cor4 median_sentiment_score     -0.0026 0.972689          No
# 
# === KEY FINDINGS ===
# 
# ✓ Strongest POSITIVE correlation: total_reviews (r = 0.476, p = 0.000000)
# ✓ Strongest NEGATIVE correlation: positive_ratio (r = -0.099, p = 0.188791)
# 
# ✓ 5 out of 9 metrics show statistically significant correlation (p < 0.05)
# 
# === GENERATING PLOTS ===
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using formula = 'y ~ x'
# `geom_smooth()` using formula = 'y ~ x'
# 
# ✓ Saved: correlation_results.csv
# 
# ========================================
# === PART 3: WORD CLOUD FOR CONTROVERSIAL GAME ===
# ========================================
# 
# Loading required package: NLP
# 
# Attaching package: ‘NLP’
# 
# The following object is masked from ‘package:ggplot2’:
# 
#     annotate
# 
# Loading required package: RColorBrewer
# Most Controversial Game (Highest SD = Most Divided Opinions):
#   Game title: POSTAL 2 
#   Positive Ratio: 46.1 %
#   Sentiment SD: 2.634 (higher = more divided)
#   Total reviews: 1000 
# 
# Reviews found for this game: 1000 
# Very positive reviews (score >= 4): 175 
# Very negative reviews (score <= -4): 122 
#                                                        word freq
# good                                                   good   51
# fun                                                     fun   44
# great                                                 great   43
# feel                                                   feel   27
# love                                                   love   25
# funni                                                 funni   23
# best                                                   best   21
# postal                                               postal   19
# ever                                                   ever   18
# game                                                   game   15
# play                                                   play   12
# famili                                               famili   10
# peopl                                                 peopl   10
# recommend                                         recommend    9
# ...
#                  word freq
# piss             piss   61
# peopl           peopl   35
# kill             kill   28
# fuck             fuck   14
# postal         postal   11
# got               got    9
# cop               cop    8
# gun               gun    8
# mouth           mouth    7
# ass               ass    6
# ...
# 
# 
#   transformation drops documents
# pdf 
#   2 
# 
# ✓ Word cloud saved: data/plots/sentiment_analysis_output/controversial_game_wordcloud.png
# 
# === EXAMPLE REVIEWS FROM POSTAL 2 ===
# 
# --- VERY POSITIVE (score >= 3) ---
# Score: 5.0 | this game can't be good for me but i feel great!
# ---
# Score: 3.8 | "I Know what you're thinking, but the funny thing is, I don't even like video games."
# ---
# Score: 5.0 | im addicted i love this game its so fun
# ---
# 
# --- VERY NEGATIVE (score <= -3) ---
# Score: -3.6 | Pissed on a RWS employee and he started throwing it back
# ---
# Score: -5.0 | guns dont kill people i do
# ---
# Score: -5.0 | ts game so fucking peak ong
# ---
# 
# ========================================
# ========== ALL ANALYSIS COMPLETE! ==========
# ========================================
# 
# FILES CREATED:
# 
# --- Part 1 (Sentiment Analysis) ---
# 1. data/plots/sentiment_analysis_output/game_sentiment_summary_full.csv
# 2. data/plots/sentiment_analysis_output/review_sentiment_sample.csv
# 3. data/plots/sentiment_analysis_output/positive_ratio_distribution.png
# 4. data/plots/sentiment_analysis_output/avg_sentiment_normalized.png
# 
# --- Part 2 (Correlation Analysis) ---
# 5. data/plots/sentiment_analysis_output/merged_sentiment_sales.csv
# 6. data/plots/sentiment_analysis_output/correlation_results.csv
# 7. data/plots/sentiment_analysis_output/correlation_posratio_vs_sales.png
# 8. data/plots/sentiment_analysis_output/correlation_avgsentiment_vs_sales.png
# 9. data/plots/sentiment_analysis_output/correlation_sd_vs_sales.png
# 10. data/plots/sentiment_analysis_output/correlation_reviews_vs_sales.png
# 
# --- Part 3 (Word Cloud for Controversial Game) ---
# 11. data/plots/sentiment_analysis_output/controversial_game_wordcloud.png
# 
# KEY INSIGHT: If sd_sentiment_score (controversy) correlates with sales,
#              the word cloud shows what fans vs critics say about the game.
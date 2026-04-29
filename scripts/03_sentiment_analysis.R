# ============================================
# COMPLETE STEAM GAME SENTIMENT ANALYSIS
# Using Cleaned Reviews + AFINN (WITH NORMALIZATION)
# ============================================

library(syuzhet)
library(dplyr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)

plot_output_dir <- "data/plots/sentiment_analysis_output"
dir.create(plot_output_dir, recursive = TRUE, showWarnings = FALSE)

# ============================================
# Part 1: Load Data
# ============================================

cat("=== LOADING DATA ===\n")
steam_data <- read.csv("/Users/user/Downloads/steam_game_reviews_730945 2.csv") 
cat("Total raw reviews:", nrow(steam_data), "\n")

# Load master dataset
master_data <- read.csv("data/processed/master_dataset.csv")

if(!"appid" %in% names(steam_data)) {
  stop("Column 'appid' not found in steam review data.")
}

if(!"steamid" %in% names(master_data)) {
  stop("Column 'steamid' not found in master_dataset.csv.")
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
cat("Reviews after filter:", nrow(steam_data), "(removed", total_reviews_before_filter - nrow(steam_data), ")\n")
cat("Unique games after filter:", length(unique(steam_data$name)), "\n")

if(nrow(steam_data) == 0) {
  stop("No review rows matched master_dataset steamids.")
}

# ============================================
# Part 2: Function to Clean Reviews
# ============================================

clean_review <- function(text) {
  text <- as.character(text)
  text <- gsub("[\r\n\t]", " ", text)
  text <- gsub("http\\S+|www\\S+", "", text)
  text <- gsub("<.*?>", "", text)
  text <- iconv(text, "UTF-8", "ASCII", sub = " ")
  text <- gsub("[^a-zA-Z\\s']", " ", text)
  text <- tolower(text)
  text <- gsub("\\s+", " ", text)
  text <- trimws(text)
  return(text)
}

# ============================================
# Part 3: Clean All Reviews
# ============================================

cat("\n=== CLEANING REVIEWS ===\n")
cat("Reviews to clean:", nrow(steam_data), "\n")

steam_data$clean_review <- sapply(steam_data$review, clean_review)

cat("✓ Cleaning complete!\n")

# ============================================
# Part 4: Apply AFINN Sentiment (LOG TRANSFORM × k=5)
# Formula: log(|raw| + 1) × sign(raw) × k
# ============================================

cat("\n=== APPLYING AFINN SENTIMENT (LOG TRANSFORM × k=5) ===\n")

raw_scores <- get_sentiment(steam_data$clean_review, method = "afinn")

steam_data$word_count <- sapply(strsplit(steam_data$clean_review, " "), length)

k <- 5
steam_data$sentiment_score <- log(abs(raw_scores) + 1) * sign(raw_scores) * k

steam_data$sentiment_score[is.na(steam_data$sentiment_score)] <- 0
steam_data$sentiment_score[is.infinite(steam_data$sentiment_score)] <- 0

steam_data$is_positive <- steam_data$sentiment_score > 0
steam_data$sentiment_category <- ifelse(
  steam_data$sentiment_score > 0, "Positive",
  ifelse(steam_data$sentiment_score < 0, "Negative", "Neutral")
)

cat("\n=== LOG TRANSFORM (k=5) RESULTS ===\n")
cat("Raw score range:", min(raw_scores), "to", max(raw_scores), "\n")
cat("Normalized score range:", 
    round(min(steam_data$sentiment_score, na.rm = TRUE), 4), "to",
    round(max(steam_data$sentiment_score, na.rm = TRUE), 4), "\n")
cat("Mean normalized score:", round(mean(steam_data$sentiment_score, na.rm = TRUE), 4), "\n")
cat("Positive reviews:", sum(steam_data$sentiment_score > 0, na.rm = TRUE), "\n")
cat("Negative reviews:", sum(steam_data$sentiment_score < 0, na.rm = TRUE), "\n")
cat("Neutral reviews:", sum(steam_data$sentiment_score == 0, na.rm = TRUE), "\n")

# ============================================
# Part 5: Create Game Summary
# ============================================

cat("\n=== CREATING GAME SUMMARY ===\n")

game_summary <- steam_data %>%
  mutate(appid = as.character(appid)) %>%
  group_by(appid) %>%
  summarise(
    game_title = first(name),
    total_reviews = n(),
    positive_count = sum(is_positive, na.rm = TRUE),
    negative_count = sum(sentiment_category == "Negative", na.rm = TRUE),
    neutral_count = sum(sentiment_category == "Neutral", na.rm = TRUE),
    positive_ratio = positive_count / total_reviews * 100,
    negative_ratio = negative_count / total_reviews * 100,
    neutral_ratio = neutral_count / total_reviews * 100,
    avg_sentiment_score = mean(sentiment_score, na.rm = TRUE),
    median_sentiment_score = median(sentiment_score, na.rm = TRUE),
    sd_sentiment_score = sd(sentiment_score, na.rm = TRUE),
    very_positive_count = sum(sentiment_score >= 3, na.rm = TRUE),
    very_negative_count = sum(sentiment_score <= -3, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_reviews))

write.csv(game_summary, file.path(plot_output_dir, "game_sentiment_summary_full.csv"), row.names = FALSE)
cat("✓ Saved: game_sentiment_summary_full.csv\n")
cat("  Total games in summary:", nrow(game_summary), "\n")

# ============================================
# Part 6: Create Merged Data for Analysis
# ============================================

cat("\n=== CREATING MERGED DATA FOR ANALYSIS ===\n")

master_sales <- master_data %>%
  mutate(steamid = as.character(steamid)) %>%
  filter(!is.na(steamid), steamid != "") %>%
  select(steamid, global_sales) %>%
  distinct(steamid, .keep_all = TRUE) %>%
  rename(appid = steamid)

merged_analysis <- game_summary %>%
  inner_join(master_sales, by = "appid")

cat("Merged data size:", nrow(merged_analysis), "games\n")
cat("Games with sales data:", sum(!is.na(merged_analysis$global_sales)), "\n")

write.csv(merged_analysis, file.path(plot_output_dir, "merged_sentiment_sales.csv"), row.names = FALSE)
cat("✓ Saved: merged_sentiment_sales.csv\n")

# ============================================
# Part 7: CORRELATION ANALYSIS (All Metrics)
# ============================================

cat("\n")
cat("========================================\n")
cat("=== SENTIMENT vs SALES ANALYSIS ===\n")
cat("========================================\n\n")

analysis_data <- merged_analysis %>%
  filter(!is.na(global_sales), global_sales > 0)

cat("Games in analysis:", nrow(analysis_data), "\n\n")

# Correlation for ALL metrics
metrics <- c(
  "positive_ratio",
  "negative_ratio",
  "neutral_ratio",
  "avg_sentiment_score",
  "median_sentiment_score",
  "sd_sentiment_score",
  "total_reviews",
  "very_positive_count",
  "very_negative_count"
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

# Display results
cat("=== ALL CORRELATION RESULTS ===\n")
print(correlation_results)

# Save full correlation results
write.csv(correlation_results, file.path(plot_output_dir, "correlation_all_metrics.csv"), row.names = FALSE)
cat("\n✓ Saved: correlation_all_metrics.csv\n")

# Identify Top 2 Metrics
top2 <- correlation_results %>%
  head(2)

cat("\n=== TOP 2 METRICS (Highest |r|) ===\n")
print(top2)

# Plot Top 2 Metrics vs Global Sales
cat("\n=== GENERATING TOP 2 PLOTS ===\n")

plot_correlation <- function(data, metric_name, cor_value, p_value, rank_num) {
  
  color <- ifelse(grepl("positive|total|very_positive", metric_name), "steelblue",
                   ifelse(grepl("negative|very_negative", metric_name), "darkred",
                          ifelse(grepl("neutral", metric_name), "darkorange", "purple")))
  
  p <- ggplot(data, aes_string(x = metric_name, y = "global_sales")) +
    geom_point(alpha = 0.6, color = color, size = 2.5) +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink") +
    labs(
      title = paste("Top", rank_num, "Correlation:", gsub("_", " ", toupper(metric_name)), "vs Global Sales"),
      subtitle = paste("r =", round(cor_value, 4), "| p =", round(p_value, 6)),
      x = paste(gsub("_", " ", toupper(metric_name)), "(%)"),
      y = "Global Sales (millions)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5)
    )
  
  return(p)
}

# Generate plots for top 2 metrics
for(i in 1:nrow(top2)) {
  metric_name <- top2$Metric[i]
  cor_value <- top2$Correlation[i]
  p_value <- top2$P_value[i]
  
  p <- plot_correlation(analysis_data, metric_name, cor_value, p_value, i)
  
  filename <- paste0("correlation_top", i, "_", metric_name, "_vs_sales.png")
  ggsave(file.path(plot_output_dir, filename), p, width = 8, height = 6, dpi = 300)
  
  cat(sprintf("✓ Saved: %s\n", filename))
  print(p)
}

# ============================================
# Part 8: WORD CLOUD FOR HIGHEST SD GAME (Most Controversial)
# ============================================

cat("\n")
cat("========================================\n")
cat("=== WORD CLOUD FOR MOST CONTROVERSIAL GAME ===\n")
cat("========================================\n\n")

# Step 1: Find the game with highest sd_sentiment_score
controversial_game <- game_summary %>%
  filter(total_reviews >= 100) %>%
  arrange(desc(sd_sentiment_score)) %>%
  head(1)

cat("Most Controversial Game (Highest SD = Most Divided Opinions):\n")
cat("  Game title:", controversial_game$game_title, "\n")
cat("  Positive Ratio:", round(controversial_game$positive_ratio, 1), "%\n")
cat("  Sentiment SD:", round(controversial_game$sd_sentiment_score, 3), "(higher = more divided)\n")
cat("  Total reviews:", controversial_game$total_reviews, "\n\n")

# Step 2: Extract reviews for this game
game_reviews <- steam_data %>%
  filter(name == controversial_game$game_title)

cat("Reviews found for this game:", nrow(game_reviews), "\n")

# Step 3: Split into very positive (score >= 3) and very negative (score <= -3)
very_positive <- game_reviews %>%
  filter(sentiment_score >= 15) %>%
  pull(clean_review)

very_negative <- game_reviews %>%
  filter(sentiment_score <= -15) %>%
  pull(clean_review)

cat("Very positive reviews (score >= 3):", length(very_positive), "\n")
cat("Very negative reviews (score <= -3):", length(very_negative), "\n")

# Step 4: Function to create word cloud
create_wordcloud_teacher <- function(text_vector, title_name, min_freq = 2) {
  
  if(length(text_vector) < 3) {
    cat("Not enough reviews for:", title_name, "\n")
    return(NULL)
  }
  
  combined_text <- paste(text_vector, collapse = " ")
  
  TextDoc <- Corpus(VectorSource(combined_text))
  
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
  
  TextDoc_dtm <- TermDocumentMatrix(TextDoc)
  dtm_m <- as.matrix(TextDoc_dtm)
  dtm_v <- sort(rowSums(dtm_m), decreasing = TRUE)
  dtm_d <- data.frame(word = names(dtm_v), freq = dtm_v)
  
  if(nrow(dtm_d) == 0) {
    cat("No words found for:", title_name, "\n")
    return(NULL)
  }
  
  set.seed(1234)
  wordcloud(words = dtm_d$word, freq = dtm_d$freq, min.freq = min_freq,
            max.words = 100, random.order = FALSE, rot.per = 0.40,
            colors = brewer.pal(8, "Dark2"))
  title(main = title_name, cex.main = 0.9)
  
  return(dtm_d)
}

# Step 5: Create and save side-by-side word clouds
png(filename = file.path(plot_output_dir, "controversial_game_wordcloud.png"), 
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

cat("\n✓ Word cloud saved: controversial_game_wordcloud.png\n")

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
# Summary Output
# ============================================

cat("\n")
cat("========================================\n")
cat("========== ANALYSIS COMPLETE! ==========\n")
cat("========================================\n")
cat("\n")

cat("=== KEY FINDINGS ===\n")
for(i in 1:nrow(top2)) {
  cat(sprintf("%d. %s: r = %.4f, p = %.4f (%s)\n", 
              i, 
              top2$Metric[i], 
              top2$Correlation[i], 
              top2$P_value[i],
              ifelse(top2$Significant[i] == "Yes", "Significant", "Not significant")))
}

cat(sprintf("\n3. Most controversial game: %s (SD = %.3f)\n", 
            controversial_game$game_title, 
            controversial_game$sd_sentiment_score))

cat("\n=== FILES CREATED ===\n")
cat("1. data/plots/sentiment_analysis_output/game_sentiment_summary_full.csv\n")
cat("2. data/plots/sentiment_analysis_output/merged_sentiment_sales.csv\n")
cat("3. data/plots/sentiment_analysis_output/correlation_all_metrics.csv\n")
cat("4. data/plots/sentiment_analysis_output/correlation_top1_*_vs_sales.png\n")
cat("5. data/plots/sentiment_analysis_output/correlation_top2_*_vs_sales.png\n")
cat("6. data/plots/sentiment_analysis_output/controversial_game_wordcloud.png\n")
cat("\n✓ Analysis complete!\n")

library(readr)
library(dplyr)
library(rpart)
library(rpart.plot)
library(caret)

# Master dataset 
master_data <- read_csv("master_dataset.csv")

# Data Preparation 
# Focus on attributes known at the time of "decision", pre-launch
cart_df <- master_data %>%
  select(
    commercial_success, 
    specific_genre, 
    platform, 
    release_period, 
    competitors, 
    market_share_pct
  ) %>%
  # Convert categorical variables to factors for rpart
  mutate(across(where(is.character), as.factor)) %>%
  # Remove rows w/ missing targets for training
  filter(!is.na(commercial_success))

# 3. Training & Testing Split
set.seed(42)
train_index <- createDataPartition(cart_df$commercial_success, p = 0.7, list = FALSE)
train_data <- cart_df[train_index, ]
test_data  <- cart_df[-train_index, ]

# Build CART 
# Commercial Success is a categorical outcome (High/Low), use class
fit <- rpart(commercial_success ~ ., 
             data = train_data, 
             method = "class", 
             control = rpart.control(cp = 0.002)) 

# Pruning the Tree for accuracy
# Identify the best complexity parameter to minimise cross-validation error
best_cp <- fit$cptable[which.min(fit$cptable[,"xerror"]),"CP"]
pruned_fit <- prune(fit, cp = best_cp)

# Visualisation
prp(pruned_fit, 
    extra = 104, 
    box.palette = "RdYlGn", 
    main = "Final CART: Predictors of Commercial Success",
    type = 4, 
    fallen.leaves = TRUE)

# Model Evaluation
# Predict on the unseen Test Data
predictions <- predict(pruned_fit, test_data, type = "class")
conf_matrix <- confusionMatrix(predictions, test_data$commercial_success)


cat("\n--- MODEL PERFORMANCE SUMMARY ---\n")
cat("Overall Accuracy:", round(conf_matrix$overall['Accuracy'] * 100, 2), "%\n")
print(conf_matrix$table)

# Importance Ranking
importance <- pruned_fit$variable.importance
importance_df <- data.frame(
  Attribute = names(importance), 
  Score = as.numeric(importance)
) %>% arrange(desc(Score))

print("\n--- ATTRIBUTE IMPORTANCE RANKING ---")
print(importance_df)

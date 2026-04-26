setwd("~/University Courses/SEMESTER 4/IIMT2641/Assignments/Group Project")
getwd()

#Libraries
install.packages(caTools)
install.packages(car)
install.packages(ROCR)
install.packages(ggplot2)

library(caTools)
library(car)
library(ROCR)
library(ggplot2)

#Data
df_raw <- read.csv("data/processed/master_dataset.csv", stringsAsFactors = FALSE)

cat("Raw dataset dimensions:", nrow(df_raw), "rows x", ncol(df_raw), "cols\n")
str(df_raw[, c("global_sales", "commercial_success", "genre",
               "specific_genre", "platform", "release_period",
               "market_share_pct", "competitors")])

#DATA CLEANING & FEATURE ENGINEERING
# Keep only rows where global_sales is observed (needed for both models)
df <- df_raw[!is.na(df_raw$global_sales), ]
cat("Rows after removing missing global_sales:", nrow(df), "\n")

#Remove the tiny 'unknown' release_period group
df <- df[df$release_period != "unknown", ]
cat("Rows after removing unknown release_period:", nrow(df), "\n")

#Platform family grouping
#Many platforms share a brand family; collapsing reduces factor levels
platform_family <- function(p) {
  ps_brands   <- c("PS", "PS2", "PS3", "PS4", "PS5", "PSP", "PSV", "PSN")
  xbox_brands <- c("XB", "X360", "XOne", "XS", "XBL")
  ninty_home  <- c("NES", "SNES", "N64", "GC", "Wii", "WiiU", "NS")
  ninty_hand  <- c("GB", "GBC", "GBA", "DS", "DSi", "DSiW", "3DS")
  pc_brands   <- c("PC", "OSX", "Linux")
  sega_brands <- c("GEN", "SAT", "DC", "GG", "MS", "S32X", "SCD", "32X")
  
  ifelse(p %in% ps_brands,   "PlayStation",
         ifelse(p %in% xbox_brands, "Xbox",
                ifelse(p %in% ninty_home,  "Nintendo_Home",
                       ifelse(p %in% ninty_hand,  "Nintendo_Handheld",
                              ifelse(p %in% pc_brands,   "PC",
                                     ifelse(p %in% sega_brands, "Sega",
                                            "Other"))))))
}

df$platform_family <- platform_family(df$platform)
cat("Platform family distribution:\n")
print(table(df$platform_family))

#Fill missing market_share_pct with the median (small proportion missing)
med_mkt <- median(df$market_share_pct, na.rm = TRUE)
df$market_share_pct[is.na(df$market_share_pct)] <- med_mkt

#Fill missing competitors with the median
med_comp <- median(df$competitors, na.rm = TRUE)
df$competitors[is.na(df$competitors)] <- med_comp

#Log-transform global_sales (right-skewed; already in dataset but we create it explicitly for clarity)
df$log_sales <- log(df$global_sales + 0.01)   # +0.01 avoids log(0)

#Convert categorical variables to factors
df$specific_genre   <- as.factor(df$specific_genre)
df$platform_family  <- as.factor(df$platform_family)
df$release_period   <- as.factor(df$release_period)

#Set meaningful reference levels
df$specific_genre  <- relevel(df$specific_genre,  ref = "action")
df$platform_family <- relevel(df$platform_family, ref = "PlayStation")
df$release_period  <- relevel(df$release_period,  ref = "2005_2014")

#Binary outcome for Logistic Regression (1 = high, 0 = low)
df$success_binary <- ifelse(df$commercial_success == "high", 1, 0)
df$success_binary <- as.factor(df$success_binary)
cat("\nClass balance for commercial_success:\n")
print(table(df$success_binary))

#Final predictor check
cat("\nFinal modelling dataset dimensions:", nrow(df), "x", ncol(df), "\n")


#TRAIN / TEST SPLIT (shared for both models)
set.seed(67)

#Stratify on success_binary to keep class balance in both sets
spl  <- sample.split(df$success_binary, SplitRatio = 0.75)
train <- subset(df, spl == TRUE)
test  <- subset(df, spl == FALSE)

cat("\nTraining rows:", nrow(train), "  |  Testing rows:", nrow(test), "\n")

# --- Price-filtered dataset (for price models only) ---
# price is missing for ~98% of rows; filter separately so the main models are unaffected
df_price <- df[!is.na(df$price), ]
cat("\nPrice-filtered dataset rows:", nrow(df_price), "(Steam games only)\n")

set.seed(67)
spl_p   <- sample.split(df_price$success_binary, SplitRatio = 0.75)
train_p <- subset(df_price, spl_p == TRUE)
test_p  <- subset(df_price, spl_p == FALSE)
cat("Price model - Training rows:", nrow(train_p), "  |  Testing rows:", nrow(test_p), "\n")


#LINEAR REGRESSION – predict log_sales (numerical)
cat("\n", strrep("=", 70), "\n")
cat("LINEAR REGRESSION\n")
cat(strrep("=", 70), "\n")

#Full model 
lm_full <- lm(log_sales ~ specific_genre + platform_family +
                release_period + market_share_pct + competitors,
              data = train)
cat("\n--- Full Model Summary ---\n")
print(summary(lm_full))

#Check multicollinearity (VIF)
cat("\n--- VIF (Full Model) ---\n")
print(vif(lm_full))
cat("\n No Multicollinearity \n")

#In-sample performance 
sse_train <- sum(lm_full$residuals^2)
sst_train <- sum((train$log_sales - mean(train$log_sales))^2)
r2_train  <- 1 - sse_train / sst_train
cat("\n--- In-Sample Performance (Training Set) ---\n")
cat(sprintf("  SSE        : %.4f\n", sse_train))
cat(sprintf("  R-squared  : %.4f\n", r2_train))
cat(sprintf("  Adj R²     : %.4f\n", summary(lm_full)$adj.r.squared))
cat(sprintf("  RMSE (log) : %.4f\n", sqrt(sse_train / nrow(train))))

#Out-of-sample (test set) prediction & evaluation
lm_pred_test <- predict(lm_full, newdata = test)

sse_test <- sum((test$log_sales - lm_pred_test)^2)
sst_test <- sum((test$log_sales - mean(train$log_sales))^2)  
r2_test  <- 1 - sse_test / sst_test
rmse_test <- sqrt(mean((test$log_sales - lm_pred_test)^2))

cat("\n--- Out-of-Sample Performance (Test Set) ---\n")
cat(sprintf("  R-squared (out-of-sample) : %.4f\n", r2_test))
cat(sprintf("  RMSE (log scale)          : %.4f\n", rmse_test))

#Back-transform to original sales scale for interpretability
test$pred_sales     <- exp(lm_pred_test) - 0.01
rmse_orig <- sqrt(mean((test$global_sales - test$pred_sales)^2))
cat(sprintf("  RMSE (original scale, M$) : %.4f\n", rmse_orig))

#Residual plots
par(mfrow = c(2, 2))
plot(lm_full, main = "Linear Regression Diagnostics")
par(mfrow = c(1, 1))

#Actual vs Predicted (log scale)
plot(test$log_sales, lm_pred_test,
     xlab = "Actual log(Global Sales)",
     ylab = "Predicted log(Global Sales)",
     main = "Linear Regression: Actual vs Predicted (Test Set)",
     pch  = 16, col = rgb(0, 0, 1, 0.3))
abline(0, 1, col = "red", lwd = 2)

#Example prediction for a new game
cat("\n--- Example: Predict sales for a new game ---\n")
new_game <- data.frame(
  specific_genre   = factor("action",       levels = levels(df$specific_genre)),
  platform_family  = factor("PlayStation",  levels = levels(df$platform_family)),
  release_period   = factor("2015_plus",    levels = levels(df$release_period)),
  market_share_pct = 8.5,   
  competitors      = 300    
)

pred_log   <- predict(lm_full, newdata = new_game)
pred_sales <- exp(pred_log) - 0.01
cat(sprintf("  Predicted log(sales) : %.4f\n", pred_log))
cat(sprintf("  Predicted sales (M$) : %.2f million\n", pred_sales))


# --- Linear Regression (Price Model) ---
cat("\n--- Price Model: Linear Regression (Steam games only, n =", nrow(train_p), ") ---\n")

lm_price <- lm(log_sales ~ specific_genre + platform_family +
                 release_period + market_share_pct + competitors + price,
               data = train_p)
print(summary(lm_price))

cat("\n--- VIF (Price Model) ---\n")
print(vif(lm_price))
cat("\n No Multicollinearity \n")

# In-sample performance
sse_train_p <- sum(lm_price$residuals^2)
sst_train_p <- sum((train_p$log_sales - mean(train_p$log_sales))^2)
r2_train_p  <- 1 - sse_train_p / sst_train_p
cat(sprintf("\n  SSE        : %.4f\n", sse_train_p))
cat(sprintf("  R-squared  : %.4f\n", r2_train_p))
cat(sprintf("  Adj R²     : %.4f\n", summary(lm_price)$adj.r.squared))
cat(sprintf("  RMSE (log) : %.4f\n", sqrt(sse_train_p / nrow(train_p))))

# Out-of-sample performance
lm_pred_price <- predict(lm_price, newdata = test_p)
sse_test_p  <- sum((test_p$log_sales - lm_pred_price)^2)
sst_test_p  <- sum((test_p$log_sales - mean(train_p$log_sales))^2)
r2_test_p   <- 1 - sse_test_p / sst_test_p
rmse_test_p <- sqrt(mean((test_p$log_sales - lm_pred_price)^2))

cat(sprintf("  R-squared (out-of-sample) : %.4f\n", r2_test_p))
cat(sprintf("  RMSE (log scale)          : %.4f\n", rmse_test_p))

# Back-transform
test_p$pred_sales_p <- exp(lm_pred_price) - 0.01
rmse_orig_p <- sqrt(mean((test_p$global_sales - test_p$pred_sales_p)^2))
cat(sprintf("  RMSE (original scale, M$) : %.4f\n", rmse_orig_p))

# Actual vs Predicted plot
plot(test_p$log_sales, lm_pred_price,
     xlab = "Actual log(Global Sales)",
     ylab = "Predicted log(Global Sales)",
     main = "Price Model - Linear Regression: Actual vs Predicted (Test Set)",
     pch  = 16, col = rgb(0.8, 0.2, 0, 0.4))
abline(0, 1, col = "red", lwd = 2)

# Example prediction
cat("\n--- Price Model Example: Predict sales for a new game ---\n")
new_game_p <- data.frame(
  specific_genre   = factor("action",      levels = levels(df$specific_genre)),
  platform_family  = factor("PC",          levels = levels(df$platform_family)),
  release_period   = factor("2015_plus",   levels = levels(df$release_period)),
  market_share_pct = 8.5,
  competitors      = 300,
  price            = 29.99
)
pred_log_p   <- predict(lm_price, newdata = new_game_p)
pred_sales_p <- exp(pred_log_p) - 0.01
cat(sprintf("  Predicted log(sales) : %.4f\n", pred_log_p))
cat(sprintf("  Predicted sales (M$) : %.2f million\n", pred_sales_p))


# =============================================================================
# LOGISTIC REGRESSION – classify commercial success (high / low)
# =============================================================================
cat("\n", strrep("=", 70), "\n")
cat("PART 2: LOGISTIC REGRESSION\n")
cat(strrep("=", 70), "\n")

#Full logistic model
logit_full <- glm(success_binary ~ specific_genre + platform_family +
                    release_period + market_share_pct + competitors,
                  data   = train,
                  family = binomial)
cat("\n--- Full Logistic Model Summary ---\n")
print(summary(logit_full))

#VIF check
cat("\n--- VIF (Full Logistic Model) ---\n")
print(vif(logit_full))
cat("\n No Multicollinearity \n")

#Odds ratios
cat("\n--- Odds Ratios (Stepwise Logistic Model) ---\n")
odds_ratio <- exp(coef(logit_full))
ci         <- exp(confint(logit_full))
or_table   <- cbind(OddsRatio = odds_ratio, ci)
print(round(or_table, 4))

#Training-set evaluation
pred_train_prob <- predict(logit_full, type = "response") 

cat("\n--- Confusion Matrix (Training Set, threshold = 0.30) ---\n")
print(table(Actual    = train$success_binary,
            Predicted = pred_train_prob > 0.30))

accuracy_train <- mean((pred_train_prob > 0.30) == (train$success_binary == 1))
cat(sprintf("Training accuracy (t=0.30): %.4f\n", accuracy_train))

#ROC Curve & AUC on training set
rocr_pred_train <- prediction(pred_train_prob, train$success_binary)
roc_train       <- performance(rocr_pred_train, "tpr", "fpr")

plot(roc_train,
     colorize       = TRUE,
     print.cutoffs.at = seq(0, 1, 0.1),
     text.adj       = c(-0.2, 1.7),
     main           = "ROC Curve – Logistic Regression (Training Set)")

auc_train <- as.numeric(performance(rocr_pred_train, "auc")@y.values)
cat(sprintf("\nAUC (Training Set): %.4f\n", auc_train))

#Test-set evaluation 
pred_test_prob <- predict(logit_full, newdata = test, type = "response")

cat("\n--- Confusion Matrix (Test Set, threshold = 0.30) ---\n")
conf_mat <- table(Actual    = test$success_binary,
                  Predicted = pred_test_prob > 0.30)
print(conf_mat)

#Accuracy, Sensitivity, Specificity
TP <- conf_mat[2, 2]; FP <- conf_mat[1, 2]
TN <- conf_mat[1, 1]; FN <- conf_mat[2, 1]

accuracy    <- (TP + TN) / sum(conf_mat)
sensitivity <- TP / (TP + FN)    
specificity <- TN / (TN + FP)    
precision   <- TP / (TP + FP)

cat("\n--- Classification Metrics (Test Set, threshold = 0.30) ---\n")
cat(sprintf("  Accuracy    : %.4f\n", accuracy))
cat(sprintf("  Sensitivity : %.4f\n", sensitivity))
cat(sprintf("  Specificity : %.4f\n", specificity))
cat(sprintf("  Precision   : %.4f\n", precision))

#Baseline accuracy (predict all "low")
baseline_acc <- mean(test$success_binary == 0)
cat(sprintf("  Baseline accuracy (all 'low'): %.4f\n", baseline_acc))

#AUC on test set
rocr_pred_test <- prediction(pred_test_prob, test$success_binary)
roc_test       <- performance(rocr_pred_test, "tpr", "fpr")

plot(roc_test,
     colorize         = TRUE,
     print.cutoffs.at = seq(0, 1, 0.1),
     text.adj         = c(-0.2, 1.7),
     main             = "ROC Curve – Logistic Regression (Test Set)")

auc_test <- as.numeric(performance(rocr_pred_test, "auc")@y.values)
cat(sprintf("\nAUC (Test Set): %.4f\n", auc_test))

#Example prediction for a new game 
cat("\n--- Example: Predict commercial success for a new game ---\n")
new_game_logit <- data.frame(
  specific_genre   = factor("action",      levels = levels(df$specific_genre)),
  platform_family  = factor("PlayStation", levels = levels(df$platform_family)),
  release_period   = factor("2015_plus",   levels = levels(df$release_period)),
  market_share_pct = 8.5,
  competitors      = 300
)

prob_success <- predict(logit_full, newdata = new_game_logit, type = "response")
class_pred   <- ifelse(prob_success > 0.30, "high", "low")

cat(sprintf("  Predicted probability of high success : %.4f\n", prob_success))
cat(sprintf("  Predicted class (t=0.30)              : %s\n",   class_pred))

# --- Logistic Regression (Price Model) ---
cat("\n--- Price Model: Logistic Regression (Steam games only, n =", nrow(train_p), ") ---\n")

logit_price <- glm(success_binary ~ specific_genre + platform_family +
                     release_period + market_share_pct + competitors + price,
                   data   = train_p,
                   family = binomial)
print(summary(logit_price))

#VIF Check
cat("\n--- VIF (Price Logistic Model) ---\n")
print(vif(logit_price))
cat("\n No Multicollinearity \n")

# Odds ratios
cat("\n--- Odds Ratios (Price Logistic Model) ---\n")
odds_ratio_p <- exp(coef(logit_price))
ci_p         <- exp(confint(logit_price))
or_table_p   <- cbind(OddsRatio = odds_ratio_p, ci_p)
print(round(or_table_p, 4))

# Training set evaluation
pred_train_prob_p <- predict(logit_price, type = "response")

cat("\n--- Confusion Matrix (Price Model, Training Set, threshold = 0.30) ---\n")
print(table(Actual    = train_p$success_binary,
            Predicted = pred_train_prob_p > 0.30))

accuracy_train_p <- mean((pred_train_prob_p > 0.30) == (train_p$success_binary == 1))
cat(sprintf("Training accuracy (t=0.30): %.4f\n", accuracy_train_p))

# ROC & AUC on training set
rocr_pred_train_p <- prediction(pred_train_prob_p, train_p$success_binary)
roc_train_p       <- performance(rocr_pred_train_p, "tpr", "fpr")
plot(roc_train_p,
     colorize         = TRUE,
     print.cutoffs.at = seq(0, 1, 0.1),
     text.adj         = c(-0.2, 1.7),
     main             = "ROC Curve – Price Model Logistic Regression (Training Set)")

auc_train_p <- as.numeric(performance(rocr_pred_train_p, "auc")@y.values)
cat(sprintf("\nAUC (Training Set): %.4f\n", auc_train_p))

# Test set evaluation
pred_test_prob_p <- predict(logit_price, newdata = test_p, type = "response")

cat("\n--- Confusion Matrix (Price Model, Test Set, threshold = 0.30) ---\n")
conf_mat_p <- table(Actual    = test_p$success_binary,
                    Predicted = pred_test_prob_p > 0.30)
print(conf_mat_p)

TP_p <- conf_mat_p[2, 2]; FP_p <- conf_mat_p[1, 2]
TN_p <- conf_mat_p[1, 1]; FN_p <- conf_mat_p[2, 1]

accuracy_p    <- (TP_p + TN_p) / sum(conf_mat_p)
sensitivity_p <- TP_p / (TP_p + FN_p)
specificity_p <- TN_p / (TN_p + FP_p)
precision_p   <- TP_p / (TP_p + FP_p)

cat("\n--- Classification Metrics (Price Model, Test Set, threshold = 0.30) ---\n")
cat(sprintf("  Accuracy    : %.4f\n", accuracy_p))
cat(sprintf("  Sensitivity : %.4f\n", sensitivity_p))
cat(sprintf("  Specificity : %.4f\n", specificity_p))
cat(sprintf("  Precision   : %.4f\n", precision_p))

baseline_acc_p <- mean(test_p$success_binary == 0)
cat(sprintf("  Baseline accuracy (all 'low'): %.4f\n", baseline_acc_p))

# ROC & AUC on test set
rocr_pred_test_p <- prediction(pred_test_prob_p, test_p$success_binary)
roc_test_p       <- performance(rocr_pred_test_p, "tpr", "fpr")
plot(roc_test_p,
     colorize         = TRUE,
     print.cutoffs.at = seq(0, 1, 0.1),
     text.adj         = c(-0.2, 1.7),
     main             = "ROC Curve – Price Model Logistic Regression (Test Set)")

auc_test_p <- as.numeric(performance(rocr_pred_test_p, "auc")@y.values)
cat(sprintf("\nAUC (Test Set): %.4f\n", auc_test_p))

# Example prediction
cat("\n--- Price Model Example: Predict commercial success for a new game ---\n")
new_game_logit_p <- data.frame(
  specific_genre   = factor("action",    levels = levels(df$specific_genre)),
  platform_family  = factor("PC",        levels = levels(df$platform_family)),
  release_period   = factor("2015_plus", levels = levels(df$release_period)),
  market_share_pct = 8.5,
  competitors      = 300,
  price            = 29.99
)
prob_success_p <- predict(logit_price, newdata = new_game_logit_p, type = "response")
class_pred_p   <- ifelse(prob_success_p > 0.30, "high", "low")
cat(sprintf("  Predicted probability of high success : %.4f\n", prob_success_p))
cat(sprintf("  Predicted class (t=0.30)              : %s\n",   class_pred_p))


#SUMMARY
cat("\n", strrep("=", 70), "\n")
cat("RESULTS SUMMARY\n")
cat(strrep("=", 70), "\n")
cat(sprintf("\n[Linear Regression]\n"))
cat(sprintf("  Training R²           : %.4f\n", r2_train))
cat(sprintf("  Test R² (out-of-sample): %.4f\n", r2_test))
cat(sprintf("  Test RMSE (log scale)  : %.4f\n", rmse_test))

cat(sprintf("\n[Logistic Regression]\n"))
cat(sprintf("  Training AUC  : %.4f\n", auc_train))
cat(sprintf("  Test AUC      : %.4f\n", auc_test))
cat(sprintf("  Test Accuracy : %.4f  (Baseline: %.4f)\n", accuracy, baseline_acc))
cat(sprintf("  Sensitivity   : %.4f\n", sensitivity))
cat(sprintf("  Specificity   : %.4f\n", specificity))

cat(sprintf("\n[Linear Regression – Price Model (Steam only, n=%d)]\n", nrow(df_price)))
cat(sprintf("  Training R²            : %.4f\n", r2_train_p))
cat(sprintf("  Test R² (out-of-sample): %.4f\n", r2_test_p))
cat(sprintf("  Test RMSE (log scale)  : %.4f\n", rmse_test_p))

cat(sprintf("\n[Logistic Regression – Price Model (Steam only, n=%d)]\n", nrow(df_price)))
cat(sprintf("  Training AUC  : %.4f\n", auc_train_p))
cat(sprintf("  Test AUC      : %.4f\n", auc_test_p))
cat(sprintf("  Test Accuracy : %.4f  (Baseline: %.4f)\n", accuracy_p, baseline_acc_p))
cat(sprintf("  Sensitivity   : %.4f\n", sensitivity_p))
cat(sprintf("  Specificity   : %.4f\n", specificity_p))

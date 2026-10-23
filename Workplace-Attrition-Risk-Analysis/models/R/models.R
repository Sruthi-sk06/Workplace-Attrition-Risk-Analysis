# ==========================================
# Workplace Attrition Risk Analysis
# Machine Learning Models
# ==========================================

library(randomForest)
library(rpart)
library(pROC)
# ------------------------------------------
# 1. Train-Test Split
# ------------------------------------------

set.seed(123)

train_index <- unlist(
  lapply(
    levels(data$Attrition),
    function(class) {
      class_indices <- which(data$Attrition == class)
      
      sample(
        class_indices,
        size = floor(0.80 * length(class_indices))
      )
    }
  )
)

train_data <- data[train_index, ]
test_data <- data[-train_index, ]

dim(train_data)
dim(test_data)

table(train_data$Attrition)
table(test_data$Attrition)
# ==========================================
# 2. Logistic Regression
# ==========================================

logistic_model <- glm(
  Attrition ~ .,
  data = train_data,
  family = binomial
)

summary(logistic_model)
logistic_prob <- predict(
  logistic_model,
  newdata = test_data,
  type = "response"
)

logistic_pred <- ifelse(
  logistic_prob >= 0.5,
  "Yes",
  "No"
)

logistic_pred <- factor(
  logistic_pred,
  levels = levels(test_data$Attrition)
)
# ------------------------------------------
# Logistic Regression Evaluation
# ------------------------------------------

logistic_cm <- table(
  Actual = test_data$Attrition,
  Predicted = logistic_pred
)

print(logistic_cm)
TP <- logistic_cm["Yes", "Yes"]
TN <- logistic_cm["No", "No"]
FP <- logistic_cm["No", "Yes"]
FN <- logistic_cm["Yes", "No"]

logistic_accuracy <- (TP + TN) / sum(logistic_cm)

logistic_precision <- TP / (TP + FP)

logistic_recall <- TP / (TP + FN)

logistic_f1 <- 2 *
  logistic_precision *
  logistic_recall /
  (logistic_precision + logistic_recall)
logistic_roc <- roc(
  test_data$Attrition,
  logistic_prob,
  levels = c("No", "Yes"),
  direction = "<"
)

logistic_auc <- auc(logistic_roc)
logistic_accuracy
logistic_precision
logistic_recall
logistic_f1
logistic_auc
# ==========================================
# 3. Decision Tree
# ==========================================

tree_model <- rpart(
  Attrition ~ .,
  data = train_data,
  method = "class"
)

print(tree_model)
tree_pred <- predict(
  tree_model,
  newdata = test_data,
  type = "class"
)
tree_cm <- table(
  Actual = test_data$Attrition,
  Predicted = tree_pred
)

print(tree_cm)
TP <- tree_cm["Yes", "Yes"]
TN <- tree_cm["No", "No"]
FP <- tree_cm["No", "Yes"]
FN <- tree_cm["Yes", "No"]

tree_accuracy <- (TP + TN) / sum(tree_cm)

tree_precision <- TP / (TP + FP)

tree_recall <- TP / (TP + FN)

tree_f1 <- 2 *
  tree_precision *
  tree_recall /
  (tree_precision + tree_recall)
tree_prob <- predict(
  tree_model,
  newdata = test_data,
  type = "prob"
)

tree_roc <- roc(
  test_data$Attrition,
  tree_prob[, "Yes"],
  levels = c("No", "Yes"),
  direction = "<"
)

tree_auc <- auc(tree_roc)
tree_accuracy
tree_precision
tree_recall
tree_f1
tree_auc
# ==========================================
# 4. Random Forest
# ==========================================

set.seed(123)

rf_model <- randomForest(
  Attrition ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

print(rf_model)
rf_pred <- predict(
  rf_model,
  newdata = test_data,
  type = "class"
)
rf_cm <- table(
  Actual = test_data$Attrition,
  Predicted = rf_pred
)

print(rf_cm)
TP <- rf_cm["Yes", "Yes"]
TN <- rf_cm["No", "No"]
FP <- rf_cm["No", "Yes"]
FN <- rf_cm["Yes", "No"]

rf_accuracy <- (TP + TN) / sum(rf_cm)

rf_precision <- TP / (TP + FP)

rf_recall <- TP / (TP + FN)

rf_f1 <- 2 *
  rf_precision *
  rf_recall /
  (rf_precision + rf_recall)
rf_prob <- predict(
  rf_model,
  newdata = test_data,
  type = "prob"
)
rf_roc <- roc(
  test_data$Attrition,
  rf_prob[, "Yes"],
  levels = c("No", "Yes"),
  direction = "<"
)

rf_auc <- auc(rf_roc)
rf_accuracy
rf_precision
rf_recall
rf_f1
rf_auc
# ==========================================
# 5. Model Comparison
# ==========================================

logistic_results <- data.frame(
  Model = "Logistic Regression",
  Accuracy = logistic_accuracy,
  Precision = logistic_precision,
  Recall = logistic_recall,
  F1 = logistic_f1,
  AUC = as.numeric(logistic_auc)
)

tree_results <- data.frame(
  Model = "Decision Tree",
  Accuracy = tree_accuracy,
  Precision = tree_precision,
  Recall = tree_recall,
  F1 = tree_f1,
  AUC = as.numeric(tree_auc)
)

rf_results <- data.frame(
  Model = "Random Forest",
  Accuracy = rf_accuracy,
  Precision = rf_precision,
  Recall = rf_recall,
  F1 = rf_f1,
  AUC = as.numeric(rf_auc)
)

model_comparison <- rbind(
  logistic_results,
  tree_results,
  rf_results
)

print(model_comparison)
write.csv(
  model_comparison,
  "results/model_comparison.csv",
  row.names = FALSE
)
# ==========================================
# 6. Feature Importance
# ==========================================

importance_values <- importance(rf_model)

print(importance_values)
importance_df <- data.frame(
  Variable = rownames(importance_values),
  importance_values,
  row.names = NULL
)

importance_df <- importance_df[
  order(
    importance_df$MeanDecreaseAccuracy,
    decreasing = TRUE
  ),
]

print(importance_df)
write.csv(
  importance_df,
  "results/random_forest_importance.csv",
  row.names = FALSE
)
# ==========================================
# 7. ROC Curves
# ==========================================

plot(
  logistic_roc,
  main = "ROC Curves - Attrition Prediction"
)

lines(
  tree_roc,
  lty = 2
)

lines(
  rf_roc,
  lty = 3
)

legend(
  "bottomright",
  legend = c(
    "Logistic Regression",
    "Decision Tree",
    "Random Forest"
  ),
  lty = c(1, 2, 3)
)
# ==========================================
# 8. Threshold Analysis
# ==========================================

calculate_metrics <- function(
    probabilities,
    actual,
    threshold
) {
  
  predictions <- ifelse(
    probabilities >= threshold,
    "Yes",
    "No"
  )
  
  predictions <- factor(
    predictions,
    levels = c("No", "Yes")
  )
  
  actual <- factor(
    actual,
    levels = c("No", "Yes")
  )
  
  cm <- table(
    Actual = actual,
    Predicted = predictions
  )
  
  TP <- cm["Yes", "Yes"]
  TN <- cm["No", "No"]
  FP <- cm["No", "Yes"]
  FN <- cm["Yes", "No"]
  
  accuracy <- (TP + TN) / sum(cm)
  
  precision <- ifelse(
    TP + FP == 0,
    0,
    TP / (TP + FP)
  )
  
  recall <- ifelse(
    TP + FN == 0,
    0,
    TP / (TP + FN)
  )
  
  f1 <- ifelse(
    precision + recall == 0,
    0,
    2 * precision * recall /
      (precision + recall)
  )
  
  data.frame(
    Threshold = threshold,
    Accuracy = accuracy,
    Precision = precision,
    Recall = recall,
    F1 = f1
  )
}
threshold_results <- do.call(
  rbind,
  lapply(
    c(0.2, 0.3, 0.4, 0.5),
    function(t) {
      calculate_metrics(
        rf_prob[, "Yes"],
        test_data$Attrition,
        t
      )
    }
  )
)

print(threshold_results)
# ==========================================
# 9. Final Prediction Threshold
# ==========================================

final_threshold <- 0.30

rf_final_pred <- ifelse(
  rf_prob[, "Yes"] >= final_threshold,
  "Yes",
  "No"
)

rf_final_pred <- factor(
  rf_final_pred,
  levels = c("No", "Yes")
)

rf_final_cm <- table(
  Actual = test_data$Attrition,
  Predicted = rf_final_pred
)

print(rf_final_cm)
TP <- rf_final_cm["Yes", "Yes"]
TN <- rf_final_cm["No", "No"]
FP <- rf_final_cm["No", "Yes"]
FN <- rf_final_cm["Yes", "No"]

final_accuracy <- (TP + TN) / sum(rf_final_cm)

final_precision <- TP / (TP + FP)

final_recall <- TP / (TP + FN)

final_f1 <- 2 *
  final_precision *
  final_recall /
  (final_precision + final_recall)

final_auc <- as.numeric(rf_auc)

final_results <- data.frame(
  Model = "Random Forest - Threshold 0.30",
  Accuracy = final_accuracy,
  Precision = final_precision,
  Recall = final_recall,
  F1 = final_f1,
  AUC = final_auc
)

print(final_results)
# ==========================================
# 10. Save Final Model
# ==========================================

final_model <- rf_model

final_model_info <- list(
  model = final_model,
  threshold = final_threshold
)

saveRDS(
  final_model_info,
  "models/attrition_final_model.rds"
)

# shapselect Usage Examples
# 
# This file demonstrates how to use the shapselect package for feature selection

library(shapselect)
library(xgboost)

# ============================================
# Example 1: Regression Task
# ============================================

cat("Example 1: Regression Task\n")
cat("================\n\n")

# Use mtcars dataset
data(mtcars)

# Prepare feature data
X <- mtcars[, -1]
X$vs <- as.factor(X$vs)
X$am <- as.factor(X$am)
y <- mtcars$mpg

# Convert to matrix for xgboost
X_matrix <- model.matrix(~ . - 1, data = X)

# Train model
cat("Training XGBoost model...\n")
model <- xgboost::xgboost(
  data = X_matrix,
  label = y,
  nrounds = 50,
  verbose = 0
)

# Feature selection
cat("Performing feature selection...\n")
result <- fsshap(
  model = model,
  x = X_matrix,
  y = y,
  threshold = 0.05
)

cat("\nFeature selection results:\n")
print(result)

cat("\nSelected features:\n")
selected_features <- result$feature_name[result$selected == 1]
print(selected_features)

# ============================================
# Example 2: Return Extended Data (including SHAP values)
# ============================================

cat("\n\nExample 2: Return Extended Data\n")
cat("====================\n\n")

result_extended <- fsshap(
  model = model,
  x = X_matrix,
  y = y,
  threshold = 0.05,
  return_extended_data = TRUE
)

cat("Result contains the following components:\n")
cat("- significance: Statistical significance results\n")
cat("- shap_values: SHAP values\n")
cat("\nFirst 5 rows of significance results:\n")
print(head(result_extended$significance, 5))

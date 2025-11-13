# shapselect Quick Start Guide

## 1. Install Dependencies

```r
install.packages(c("fastshap", "data.table", "devtools", "xgboost"))
```

## 2. Install Package

```r
library(devtools)
install("C:/Users/LucianXu/Desktop/shapselect")
```

## 3. Load Package

```r
library(shapselect)
```

## 4. Basic Usage

```r
# Prepare data
data(mtcars)
X <- mtcars[, -1]
X$vs <- as.factor(X$vs)
X$am <- as.factor(X$am)
y <- mtcars$mpg
X_matrix <- model.matrix(~ . - 1, data = X)

# Train model
library(xgboost)
model <- xgboost(
  data = X_matrix,
  label = y,
  nrounds = 50,
  verbose = 0
)

# Feature selection
result <- fs_shap(
  model = model,
  x = X_matrix,
  y = y,
  threshold = 0.05
)

# View results
print(result)

# Get selected features
selected <- result$feature_name[result$selected == 1]
print(selected)
```

## 5. Parameter Description

- `model`: Trained tree model (xgboost, lightgbm, etc.)
- `x`: Validation feature data (data frame or matrix)
- `y`: Target variable (numeric vector or column name)
- `feature_names`: Vector of feature names (optional, defaults to column names)
- `task`: Task type ("regression", "binary", "multiclass" or NULL for auto-detection)
- `threshold`: p-value threshold (default 0.05)
- `return_extended_data`: Whether to return SHAP values (default FALSE)
- `alpha`: Regularization parameter (default 1e-6)

## 6. Return Value

By default, returns a data frame with the following columns:
- `feature_name`: Feature name
- `t_value`: t-statistic
- `stat_significance`: Statistical significance (p-value)
- `coefficient`: Coefficient
- `selected`: Whether selected (1=selected, 0=not selected, -1=negatively correlated)

If `return_extended_data=TRUE`, returns a list:
- `significance`: Statistical significance results
- `shap_values`: SHAP values

## 7. Generate Documentation

```r
library(roxygen2)
roxygenize("C:/Users/LucianXu/Desktop/shapselect")
```

## 8. Check Package

```r
devtools::check("C:/Users/LucianXu/Desktop/shapselect")
```

## 9. Run Tests

```r
devtools::test("C:/Users/LucianXu/Desktop/shapselect")
```

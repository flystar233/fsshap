# shapselect

R Package for Feature Selection Based on SHAP Values and Statistical Significance

## Introduction

`shapselect` is an R package for feature selection based on SHAP (SHapley Additive exPlanations) values and statistical significance testing. The package supports regression, binary classification, and multiclass classification tasks, using iterative feature reduction methods to identify statistically significant features.

## Installation

```r
# Install from GitHub
devtools::install_github("flystar233/fs_shap")
```

## Main Features

- **fs_shap()**: Main function for feature selection based on SHAP values and statistical significance
- Supports regression, binary classification, and multiclass tasks
- Automatic task type detection
- Iterative feature reduction algorithm
- Returns detailed statistical information

## Usage Example

```r
library(shapselect)
library(xgboost)

# Prepare data
data(mtcars)
X <- mtcars[, -1]
X$vs <- as.factor(X$vs)
X$am <- as.factor(X$am)
y <- mtcars$mpg

# Convert to matrix for xgboost
X_matrix <- model.matrix(~ . - 1, data = X)

# Train model
model <- xgboost::xgboost(
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

print(result)
```

## Dependencies
- `xgboost`: For fit model
- `fastshap`: For computing SHAP values
- `data.table`: For efficient data processing
- `stats`: R base statistical functions

## References

Kraev, E., Koseoglu, B., Traverso, L., & Topiwalla, M. (2024). Shap-Select: Lightweight Feature Selection Using SHAP Values and Regression. *arXiv preprint arXiv:2410.06815*. https://arxiv.org/abs/2410.06815

## License

GPL-3

#' Fit GLM Model for Significance Testing
#'
#' Fit linear or logistic regression on SHAP features using glm
#' glm with family="gaussian" is equivalent to lm()
#'
#' @param shap_features Data frame with SHAP values
#' @param y Target vector (continuous for regression, binary for classification)
#' @param family Character: "gaussian" for regression, "binomial" for binary classification
#' @param alpha Regularization parameter (unused, kept for compatibility)
#'
#' @return Data frame with feature statistics
#' @keywords internal
fit_glm <- function(shap_features, y, family, alpha) {
  # Convert to data.frame if needed
  if (!is.data.frame(shap_features)) {
    shap_features <- as.data.frame(shap_features)
  }
  if (!is.numeric(y)) {
    stop("y must be numeric")
  }
  if (length(y) != nrow(shap_features)) {
    stop("y length (", length(y), ") must equal ",
         "nrow(shap_features) (", nrow(shap_features), ")")
  }

  # Use glm for both regression and binary classification
  fit <- glm(y ~ ., data = shap_features, family = family)
  model_type <- if (family == "gaussian") "lm" else "glm"
  return(extract_model_stats(fit, model_type))
}

#' Fit Multiclass Model
#'
#' Perform one-vs-all logistic regression for each class
#'
#' @param shap_features List of data frames with SHAP values per class
#' @param y Target vector with multiple classes
#' @param alpha Regularization parameter
#' @param return_individual Logical, return individual results
#'
#' @return Data frame with combined significance results
#' @import data.table
#' @keywords internal
fit_multiclass <- function(shap_features, y, alpha,
                          return_individual = FALSE) {
  # Declare global variables for data.table syntax
  t_value <- closeness_to_1 <- coefficient <- stat_significance <- NULL

  if (!is.list(shap_features)) {
    stop("shap_features must be a list")
  }
  if (!is.logical(return_individual) || length(return_individual) != 1L) {
    stop("return_individual must be a single logical value")
  }

  significance_dfs <- lapply(names(shap_features), function(cls) {
    feature_df <- shap_features[[cls]]
    binary_y <- as.integer(y == cls)
    fit_glm(feature_df, binary_y, "binomial", alpha)
  })

  # Combine results
  combined_df <- data.table::rbindlist(significance_dfs)

  # nolint start: no visible global function definition
  max_significance_df <- combined_df[
    , .(
      t_value = max(t_value, na.rm = TRUE),
      closeness_to_1 = min(closeness_to_1, na.rm = TRUE),
      coefficient = max(coefficient, na.rm = TRUE)
    ),
    by = "feature_name"
  ]

  # Bonferroni correction
  max_significance_df[
    , stat_significance := length(shap_features) *
      (1 - pnorm(t_value))
  ]
  # nolint end

  if (return_individual) {
    return(list(
      combined = max_significance_df,
      individual = significance_dfs
    ))
  } else {
    return(as.data.frame(max_significance_df))
  }
}


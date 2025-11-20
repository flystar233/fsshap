#' FS-SHAP - Main Feature Selection Function
#'
#' Select features based on SHAP values and statistical significance
#'
#' @param model Trained tree-based model
#' @param x Data frame with validation features
#' @param y Numeric/factor target or column name
#' @param feature_names Character vector of feature names
#' @param task Character: "regression", "binary", "multiclass", or NULL (auto-detect)
#' @param threshold Numeric: p-value threshold for selection (default 0.05)
#' @param nsim Integer: number of Monte Carlo simulations (default 100)
#' @param return_extended_data Logical: return SHAP values as well
#' @param alpha Numeric: regularization strength
#' @param parallel Logical: enable parallel processing for SHAP computation (default FALSE)
#' @param n_cores Integer: number of cores to use for parallel processing.
#'   If NULL, uses all available cores minus 1. Only used if parallel=TRUE.
#'
#' @return Data frame (or list with SHAP values if extended) with feature statistics
#'
#' @export
#' @examples
#' \dontrun{
#' library(xgboost)
#' data(mtcars)
#' # Prepare data: convert categorical variables to factors
#' # In mtcars, 'vs' and 'am' are binary categorical variables
#' X <- mtcars[, -1]
#' X$vs <- as.factor(X$vs)
#' X$am <- as.factor(X$am)
#' y <- mtcars$mpg
#'
#' # Convert to matrix for xgboost (factors will be encoded)
#' X_matrix <- model.matrix(~ . - 1, data = X)
#'
#' # Train model
#' model <- xgboost::xgboost(
#'   data = X_matrix, label = y,
#'   nrounds = 50, verbose = 0
#' )
#'
#' # For feature selection, use original data frame
#' result <- fsshap(
#'   model,
#'   X_matrix,
#'   y,
#'   threshold = 0.05
#' )
#' print(result)
#' }
#'
fsshap <- function(model,
                        x,
                        y,
                        feature_names = NULL,
                        task = NULL,
                        threshold = 0.05,
                   nsim = 100,
                        return_extended_data = FALSE,
                   alpha = 1e-6,
                   parallel = FALSE,
                   n_cores = NULL) {

  # Convert to data.table
  if (!data.table::is.data.table(x)) {
    if (is.data.frame(x)) {
      x <- data.table::as.data.table(x)
    } else {
      x <- data.table::as.data.table(as.data.frame(x))
    }
  }

  # Validate threshold
  if (!is.numeric(threshold) || length(threshold) != 1L) {
    stop("threshold must be a single numeric value")
  }
  if (threshold < 0 || threshold > 1) {
    stop("threshold must be between 0 and 1")
  }

  # Validate alpha
  if (!is.numeric(alpha) || length(alpha) != 1L) {
    stop("alpha must be a single numeric value")
  }
  if (alpha < 0) {
    stop("alpha must be >= 0")
  }

  # Validate nsim
  if (!is.numeric(nsim) || length(nsim) != 1L || nsim <= 0) {
    stop("nsim must be a positive integer")
  }
  nsim <- as.integer(nsim)

  # Validate return_extended_data
  if (!is.logical(return_extended_data) ||
      length(return_extended_data) != 1L) {
    stop("return_extended_data must be a single logical value")
  }

  # Extract y if column name provided
  if (is.character(y) && length(y) == 1L) {
    if (!y %in% colnames(x)) {
      stop("y column '", y, "' not found in x")
    }
    y <- x[[y]]
  }

  # Validate y
  if (!is.atomic(y)) {
    stop("y must be an atomic vector")
  }
  if (length(y) != nrow(x)) {
    stop("y length (", length(y), ") must equal ",
         "nrow(x) (", nrow(x), ")")
  }

  # Set feature names
  if (is.null(feature_names)) {
    feature_names <- colnames(x)
  }
  if (!is.character(feature_names)) {
    stop("feature_names must be a character vector")
  }

  # Auto-detect task
  if (is.null(task)) {
    n_unique <- length(unique(y))
    if (is.numeric(y) && n_unique > 10L) {
      task <- "regression"
    } else if (n_unique == 2L) {
      task <- "binary"
    } else {
      task <- "multiclass"
    }
  }

  # Validate task
  valid_tasks <- c("regression", "binary", "multiclass")
  if (!task %in% valid_tasks) {
    stop("task must be one of: ", paste(valid_tasks, collapse = ", "))
  }

  # Create SHAP features
  if (task == "multiclass") {
    unique_classes <- sort(unique(y))
    shap_features <- create_shap_features(
      model,
      x[, feature_names, with = FALSE],
      unique_classes,
      nsim = nsim,
      parallel = parallel
    )
  } else {
    shap_features <- create_shap_features(
      model,
      x[, feature_names, with = FALSE],
      nsim = nsim,
      parallel = parallel
    )
  }

  # Compute significance with iterative ablation
  significance_df <- iterative_reduction(
    shap_features, y, task, alpha
  )

  # Add selection column
  significance_df$selected <- as.integer(
    significance_df$stat_significance < threshold
  )
  significance_df$selected[significance_df$t_value < 0] <- -1L

  if (return_extended_data) {
    return(list(
      significance = significance_df,
      shap_values = shap_features
    ))
  } else {
    return(significance_df[, c(
      "feature_name", "t_value", "stat_significance",
      "coefficient", "selected"
    )])
  }
}

#' Create SHAP Features
#'
#' Generates SHAP values for a tree-based model on validation data
#'
#' @param model Trained tree-based model (xgboost, lightgbm)
#' @param x Data frame with validation features
#' @param classes Character vector of class names (for multiclass)
#' @param nsim Integer: number of Monte Carlo simulations
#' @param parallel Logical: enable parallel processing
#'
#' @return Data frame or list of data frames with SHAP values
#' @import fastshap
#' @keywords internal
create_shap_features <- function(model, x, classes = NULL, nsim = 100, parallel = FALSE) {
  if (parallel) {
    library(doParallel)
    registerDoParallel(cores = 12)
  }

  x_mat <- as.matrix(x)
  feature_names <- colnames(x)

  # Compute SHAP values
  if (is.null(classes)) {
    # Binary/Regression
    shap_values <- fastshap::explain(
      object = model,
      X = x_mat,
      pred_wrapper = function(object, newdata) {
        predict(object, newdata = if (!is.matrix(newdata)) as.matrix(newdata) else newdata)
      },
      nsim = nsim,
      parallel = parallel
    )
    
    result <- data.table::data.table(shap_values, check.names = FALSE)
    colnames(result) <- feature_names
    return(result)
  }
  
  # Multiclass: compute SHAP for each class
  n_classes <- length(classes)
  shap_list <- lapply(seq_len(n_classes), function(i) {
    fastshap::explain(
      object = model,
      X = x_mat,
      pred_wrapper = function(object, newdata) {
        if (!is.matrix(newdata)) newdata <- as.matrix(newdata)
        pred <- predict(object, newdata = newdata, reshape = TRUE)
        if (!is.matrix(pred)) {
          pred <- matrix(pred, nrow = nrow(newdata), ncol = n_classes, byrow = TRUE)
        }
        pred[, i]
      },
      nsim = nsim,
      parallel = parallel
    )
  })
  
  # Convert to list of data.tables
  result <- lapply(seq_len(n_classes), function(i) {
    dt <- data.table::data.table(shap_list[[i]], check.names = FALSE)
    colnames(dt) <- feature_names
    dt
  })
  names(result) <- classes
  return(result)
}

#' Extract Model Statistics
#'
#' Extract coefficients and statistics from fitted model
#'
#' @param fit Fitted model object (glm or lm)
#' @param model_type Character: "glm" or "lm"
#'
#' @return Data frame with feature statistics
#' @keywords internal
extract_model_stats <- function(fit, model_type = c("glm", "lm")) {
  # Declare global variables for data.table
  closeness_to_1 <- coefficient <- feature_name <- NULL

  model_type <- match.arg(model_type)
  summary_stats <- summary(fit)$coefficients

  p_col <- if (model_type == "glm") "Pr(>|z|)" else "Pr(>|t|)"
  t_col <- if (model_type == "glm") "z value" else "t value"

  result_df <- data.table::data.table(
    feature_name = rownames(summary_stats),
    coefficient = summary_stats[, "Estimate"],
    stderr = summary_stats[, "Std. Error"],
    stat_significance = summary_stats[, p_col],
    t_value = summary_stats[, t_col]
  )

  result_df[, closeness_to_1 := abs(coefficient - 1.0)]
  result_df <- result_df[feature_name != "(Intercept)"]

  return(result_df)
}

#' SHAP Features to Significance
#'
#' Determine task type and compute significance for all features
#'
#' @param shap_features Data frame(s) with SHAP values
#' @param y Target vector
#' @param task Character: "regression", "binary", or "multiclass"
#' @param alpha Regularization parameter
#'
#' @return Data frame sorted by t-value (descending)
#' @keywords internal
shap_features_to_significance <- function(shap_features, y, task, alpha) {
  valid_tasks <- c("regression", "binary", "multiclass")
  if (!task %in% valid_tasks) {
    stop("task must be one of: ", paste(valid_tasks, collapse = ", "))
  }
  result_df <- switch(task,
    regression = fit_glm(shap_features, y, "gaussian", alpha),
    binary = fit_glm(shap_features, y, "binomial", alpha),
    multiclass = fit_multiclass(shap_features, y, alpha)
  )

  result_df <- result_df[order(result_df$t_value, decreasing = TRUE), ]
  rownames(result_df) <- NULL

  return(result_df)
}

#' Remove Feature from SHAP Features
#'
#' Remove a feature from data frame or list of data frames
#'
#' @param shap_features Data frame or list of data frames
#' @param feature_name Character: name of feature to remove
#'
#' @return Data frame or list with feature removed
#' @keywords internal
remove_feature <- function(shap_features, feature_name) {
  if (data.table::is.data.table(shap_features)) {
    keep_cols <- setdiff(names(shap_features), feature_name)
    return(shap_features[, keep_cols, with = FALSE])
  } else {
    # Assume list of data.tables
    lapply(shap_features, function(df) {
      if (data.table::is.data.table(df)) {
        keep_cols <- setdiff(names(df), feature_name)
        df[, keep_cols, with = FALSE]
      } else {
        # Fallback for regular list of data.frames
      df[, setdiff(colnames(df), feature_name), drop = FALSE]
      }
    })
  }
}

#' Iterative Feature Reduction
#'
#' Recursively remove least significant features
#'
#' @param shap_features Data frame(s) with SHAP values
#' @param y Target vector
#' @param task Character: task type
#' @param alpha Regularization parameter
#'
#' @return Data frame with feature elimination sequence
#' @import data.table
#' @keywords internal
iterative_reduction <- function(shap_features, y, task,
                                alpha = 1e-6) {
  collected_rows <- list()

  while (TRUE) {
    significance_df <- shap_features_to_significance(
      shap_features, y, task, alpha
    )

    if (all(is.na(significance_df$t_value))) {
      collected_rows <- c(collected_rows, list(significance_df))
      break
    }

    # Find row with minimum t-value
    min_idx <- which.min(significance_df$t_value)
    min_row <- significance_df[min_idx, ]

    # Optimize list operation
    collected_rows <- c(collected_rows, list(min_row))

    # Remove feature using helper function
    feature_to_remove <- min_row$feature_name
    shap_features <- remove_feature(shap_features, feature_to_remove)

    # Check if any features left
    n_features <- if (is.data.frame(shap_features)) {
      ncol(shap_features)
    } else {
      ncol(shap_features[[1L]])
    }

    if (!n_features) break
  }

  result_df <- data.table::rbindlist(collected_rows, fill = TRUE)
  result_df <- result_df[order(result_df$t_value, decreasing = TRUE), ]

  return(result_df)
}


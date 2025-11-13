test_that("shap_select validates inputs correctly", {
  # Test threshold validation
  expect_error(
    shap_select(model = NULL, x = data.frame(), y = c(1, 2), threshold = -1),
    "threshold must be between 0 and 1"
  )
  
  expect_error(
    shap_select(model = NULL, x = data.frame(), y = c(1, 2), threshold = 2),
    "threshold must be between 0 and 1"
  )
  
  # Test alpha validation
  expect_error(
    shap_select(model = NULL, x = data.frame(), y = c(1, 2), alpha = -1),
    "alpha must be >= 0"
  )
})

test_that("shap_select handles task auto-detection", {
  # This is a placeholder test
  # Actual implementation would require a trained model
  skip("Requires trained model")
})


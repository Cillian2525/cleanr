# Basic test to check that detect_outliers returns a logical vector of the correct length
test_that("detect_outliers returns logical vector", {
  x <- c(1, 2, 3)
  result <- detect_outliers(x)
  expect_type(result, "logical")
  expect_length(result, 3)
})

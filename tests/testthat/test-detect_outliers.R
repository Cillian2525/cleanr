# Basic tests for detect_outliers
test_that("detect_outliers returns logical vector of correct length", {
  x <- c(1, 2, 3)
  result <- detect_outliers(x)
  expect_type(result, "logical")
  expect_length(result, length(x))
})

test_that("detect_outliers flags extreme values as outliers", {
  x <- c(1, 2, 3, 100)
  result <- detect_outliers(x)
  expect_true(result[4])     # 100 should be an outlier
  expect_false(result[1])    # 1 should not be an outlier
})

test_that("detect_outliers handles NA values", {
  x <- c(1, 2, 3, 4, 5, NA, 1000)
  result <- detect_outliers(x)
  expect_true(result[7])     # still detects outlier
})

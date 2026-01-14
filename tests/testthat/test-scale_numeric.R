# Basic tests for scale_numeric
test_that("scale_numeric scales a numeric vector", {
  x <- c(1, 5, 10)
  result <- scale_numeric(x)

  expect_type(result, "double")
  expect_equal(mean(result), 0)        # mean should be 0
  expect_equal(stats::sd(result), 1)   # sd should be 1
})

test_that("scale_numeric returns original vector when sd is zero", {
  x <- c(5, 5, 5)
  result <- scale_numeric(x)

  expect_equal(result, x)              # no scaling possible
})

test_that("scale_numeric scales numeric columns in a data frame", {
  df <- data.frame(
    a = 1:5,
    b = c(10, 20, 30, NA, 40),
    c = letters[1:5]
  )

  result <- scale_numeric(df)

  expect_true(is.data.frame(result))
  expect_equal(mean(result$a), 0)
  expect_equal(stats::sd(result$a), 1)
  expect_equal(df$c, result$c)          # non-numeric column unchanged
})

test_that("scale_numeric scales only specified columns", {
  df <- data.frame(
    a = 1:5,
    b = 6:10
  )

  result <- scale_numeric(df, cols = "b")

  expect_equal(result$a, df$a)          # a unchanged
  expect_equal(mean(result$b), 0)        # b scaled
})

test_that("scale_numeric handles NA values correctly", {
  x <- c(1, 2, NA, 4)

  result <- scale_numeric(x, na.rm = TRUE)

  expect_type(result, "double")
  expect_true(anyNA(result))             # NA preserved
})

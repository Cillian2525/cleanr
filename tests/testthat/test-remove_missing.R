# Basic tests for remove_missing
test_that("remove_missing removes rows with missing values by default", {
  df <- data.frame(
    a = c(1, 2, NA),
    b = c(4, 5, 6)
  )

  result <- remove_missing(df)

  expect_equal(nrow(result), 2)
  expect_false(anyNA(result))
})

test_that("remove_missing removes rows with missing values in specified columns", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(NA, 2, 3)
  )

  result <- remove_missing(df, cols = "a")

  expect_equal(nrow(result), 2)
  expect_false(anyNA(result$a))   # column a has no missing values
})

test_that("remove_missing works with multiple specified columns", {
  df <- data.frame(
    a = c(1, NA, 3),
    b = c(1, 2, NA)
  )

  result <- remove_missing(df, cols = c("a", "b"))

  expect_equal(nrow(result), 1)   # only fully complete row remains
})

test_that("remove_missing errors when columns do not exist", {
  df <- data.frame(a = 1:3)

  expect_error(
    remove_missing(df, cols = "b")   # b is not a column
  )
})

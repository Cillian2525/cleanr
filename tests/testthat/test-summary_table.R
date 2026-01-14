# Basic tests for summary_table
test_that("summary_table returns a tibble", {
  df <- data.frame(
    a = c(1, 2, 3),
    b = c("X", "Y", "X")
  )

  result <- summary_table(df)

  expect_true(tibble::is_tibble(result))
})

test_that("summary_table summarizes numeric variables correctly", {
  df <- data.frame(
    x = c(1, 2, 3, 4)
  )

  result <- summary_table(df)

  expect_true(all(result$type == "numeric"))
  expect_equal(result$n, 4)
  expect_equal(result$mean, mean(df$x))
})

test_that("summary_table summarizes categorical variables correctly", {
  df <- data.frame(
    x = c("A", "A", "B")
  )

  result <- summary_table(df)

  expect_true(all(result$type == "categorical"))
  expect_equal(sum(result$n), 3)     # total count matches rows
})

test_that("summary_table works with selected columns", {
  df <- data.frame(
    a = 1:5,
    b = letters[1:5]
  )

  result <- summary_table(df, cols = "a")

  expect_equal(unique(result$variable), "a")
})

test_that("summary_table errors when input is not a data frame", {
  expect_error(
    summary_table(1:5)
  )
})

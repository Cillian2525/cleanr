#' Detect outliers in numeric data
#'
#' Identify outliers in a numeric vector using the interquartile range (IQR) rule.
#' Observations lying below Q1 − 1.5 × IQR or above Q3 + 1.5 × IQR are
#' classified as outliers.
#'
#' @param x A numeric vector.
#'
#' @return A logical vector of the same length as \code{x}, indicating
#'   which observations are classified as outliers.
#'
#' @examples
#' detect_outliers(c(1, 2, 3, 4, 100))
#'
#' x <- c(5, 6, 7, 8, NA, 50)
#' detect_outliers(x)
#'
#' @export
detect_outliers <- function(x) {
  stopifnot(is.numeric(x))

  # handle case where all values are NA
  if (all(is.na(x))) {
    return(rep(FALSE, length(x)))
  }

  q1 <- stats::quantile(x, 0.25, na.rm = TRUE)
  q3 <- stats::quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1

  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr

  x < lower | x > upper
}

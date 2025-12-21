#' Detect outliers in numeric data
#'
#' Detects outliers using a simple statistical rule.
#'
#' @param x A numeric vector.
#'
#' @return A logical vector indicating outliers.
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


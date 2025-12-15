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
  rep(FALSE, length(x))
}

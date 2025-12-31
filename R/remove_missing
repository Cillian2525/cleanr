#' Remove rows with missing values
#'
#' This function is an S3 generic with a method for data frames.
#'
#' @param x A data frame.
#' @param cols Vector of column names to check for missing values.
#' If the vector is empty, all columns will be checked.
#'
#' @return A data frame without missing values, since the rows containing them have been removed.
#'
#' @examples
#' # As an example, we create a data frame with different columns: the first letter of our names,
#' # sports, colors and numbers.
#' # The data frame includes some missing values to demonstrate that this function works correctly.
#'
#' df <- data.frame(
#'   names = c("M", "C", "B", "J"),
#'   sports = c("Football", "Basketball", NA, "Soccer"),
#'   colors = c("Blue", NA, "White", "Black"),
#'   numbers = c(1, 2, 3, NA)
#' )
#'
#' # Check all columns
#' remove_missing(df)
#'
#' # Check only specific columns
#' remove_missing(df, cols = "sports")
#' remove_missing(df, cols = c("sports", "numbers"))
#'
#' @export
remove_missing <- function(x, cols = NULL) {
  UseMethod("remove_missing")
}

#' @rdname remove_missing
#' @export
remove_missing.data.frame <- function(x, cols = NULL) {
  if (is.null(cols)) {
    return(stats::na.omit(x))
  }
  if (!all(cols %in% names(x))) {
    stop("One or more columns in 'cols' are not in the data frame.")
  }
  x[stats::complete.cases(x[, cols, drop = FALSE]), ]
}

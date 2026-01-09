#' Scale numeric variables
#'
#' Scale numeric values using z-scores (mean 0, sd 1).
#' This works on a numeric vector, or on numeric columns in a data frame.
#'
#' @param x A numeric vector or a data frame.
#' @param cols For data frames: columns to scale (character names or indices).
#'   If NULL, all numeric columns are scaled.
#' @param na.rm Logical; if TRUE, ignore missing values when computing mean/sd.
#'
#' @return A scaled numeric vector, or a data frame with scaled numeric columns.
#'
#' @examples
#' scale_numeric(c(1, 5, 10))
#'
#' df <- data.frame(a = 1:5, b = c(10, 20, 30, NA, 40), c = letters[1:5])
#' scale_numeric(df)
#' scale_numeric(df, cols = "b")
#'
#' @export
scale_numeric <- function(x, cols = NULL, na.rm = TRUE, ...) {
  UseMethod("scale_numeric")
}

#' @method scale_numeric numeric
#' @export
scale_numeric.numeric <- function(x, na.rm = TRUE, ...) {
  m <- mean(x, na.rm = na.rm)
  s <- stats::sd(x, na.rm = na.rm)

  if (is.na(s) || s == 0) return(x)

  (x - m) / s
}

#' @method scale_numeric data.frame
#' @export
scale_numeric.data.frame <- function(x, cols = NULL, na.rm = TRUE, ...) {
  if (is.null(cols)) {
    cols <- which(vapply(x, is.numeric, logical(1)))
  } else if (is.character(cols)) {
    cols <- match(cols, names(x))
  }

  cols <- cols[!is.na(cols)]

  for (i in cols) {
    if (is.numeric(x[[i]])) {
      x[[i]] <- scale_numeric(x[[i]], na.rm = na.rm)
    }
  }

  x
}

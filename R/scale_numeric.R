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
scale_numeric <- function(x, cols = NULL, na.rm = TRUE) {
  UseMethod("scale_numeric")
}

#' @export
scale_numeric.numeric <- function(x, na.rm = TRUE, ...) {
  m <- mean(x, na.rm = na.rm)
  s <- stats::sd(x, na.rm = na.rm)

  if (is.na(s) || s == 0) return(x)

  (x - m) / s
}

#' @export
scale_numeric.data.frame <- function(x, cols = NULL, na.rm = TRUE, ...) {
  if (is.null(cols)) {
    cols <- which(vapply(x, is.numeric, logical(1)))
  }


  for (nm in cols) {
    if (is.numeric(x[[nm]])) {
      x[[nm]] <- scale_numeric(x[[nm]], na.rm = na.rm)
    }
  }

  x
}

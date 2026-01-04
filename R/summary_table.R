#' Summarize variables in a data frame.
#'
#' Create a summary table of variables in a data frame.
#' For numeric variables, computes mean, sd and quantiles.
#' For categorical variables, reports counts and proportions by level.
#'
#' @param x A data frame.
#' @param cols For data frames: columns to summarize (character names or indices).
#'   If NULL, all columns are summarized.
#' @param na.rm Logical; if TRUE, ignore missing values when computing summaries.
#'
#' @return A tidy summary table (tibble) of variables.
#'
#' @examples
#' df <- data.frame(a = c("A", "B", "C"), b = c(1, 5, 10, 20, 50), c = c("S", "M", "L", "XL"))
#' summary_table(df)
#' summary_table(df, cols = c("a", "b"))
#'
#' @export
summary_table <- function(x, cols = NULL, na.rm = TRUE){
  stopifnot(is.data.frame(x))
  UseMethod("summary_table")
}

#' @export
summary_table.data.frame <- function(x, cols = NULL, na.rm = TRUE, ...){
  if (!is.null(cols)) {
    if (!is.numeric(cols)) {
      cols <- match(cols, names(x))
    }
    cols <- cols[!is.na(cols)]
    x <- x[cols]
  }

  num_out <- lapply(names(x), function(nm) {
    v <- x[[nm]]
    if (!is.numeric(v)) return(NULL)

    qs <- stats::quantile(v, probs = c(0.25, 0.5, 0.75),
                          na.rm = na.rm, names = FALSE)

    tibble::tibble(
      variable = nm,
      type     = "numeric",
      level    = NA_character_,
      n        = sum(!is.na(v)),
      prop     = NA_real_,
      mean     = mean(v, na.rm = na.rm),
      sd       = stats::sd(v, na.rm = na.rm),
      q25      = qs[1],
      median   = qs[2],
      q75      = qs[3]
    )
  })

  cat_out <- lapply(names(x), function(nm) {
    v <- x[[nm]]
    if (!(is.factor(v) || is.character(v))) return(NULL)

    v_fac <- factor(v)
    tab   <- table(v_fac, useNA = "no")
    p     <- prop.table(tab)

    tibble::tibble(
      variable = nm,
      type     = "categorical",
      level    = names(tab),
      n        = as.integer(tab),
      prop     = as.numeric(p),
      mean     = NA_real_,
      sd       = NA_real_,
      q25      = NA_real_,
      median   = NA_real_,
      q75      = NA_real_
    )
  })

  tibble::as_tibble(do.call(rbind, c(num_out, cat_out)))
}

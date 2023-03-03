# Data structures for harp - allow dispatch to correct methods
#

# The foundational structure is the harp data frame: harp_df
# and from this other class are derived based on the content
# of the data frame

# Helper function to determine depth of a list
depth <- function(x,x_depth = 0){
  if (!is.list(x)) {
    return(x_depth)
  } else {
    return(max(unlist(lapply(x, depth, x_depth = x_depth + 1))))
  }
}

#' Coerce to a harp_df data frame
#'
#' harp uses classes to dispatch data to the correct methods when running a
#' function. \code{as_harp_df} takes a data frame and scans the column names
#' and types to assign the correct classes. If the input data frame is not a
#' \link[tibble]{tibble} it will be coerced into one to ensure an easier to
#' read print method.
#'
#' @param x a data frame with a `validdate` column.
#'
#' @return a data frame with the appropriate class
#' @export
#'
#' @examples
#' library(dplyr)
#' d_f <- data.frame(validdate = as_datetime(seq_dates(2021010100, 2021010123)))
#' as_harp_df(d_f)
#' as_harp_df(mutate(d_f, fcst_det = runif(24)))
#' as_harp_df(mutate(d_f, fcst_mbr000 = runif(24), fcst_mbr001 = runif(24)))
#' # Note the class
#' class(as_harp_df(d_f))
#' class(as_harp_df(mutate(d_f, fcst_det = runif(24))))
#' class(as_harp_df(mutate(d_f, fcst_mbr000 = runif(24), fcst_mbr001 = runif(24))))
as_harp_df <- function(x) {
  UseMethod("as_harp_df")
}

#' @export
as_harp_df.data.frame <- function(x) {
  has_validdate <- is.element("validdate", colnames(x))
  if (!has_validdate) {
    stop("Data frame must have `validdate` column.")
  }
  if (!tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }
  classes <- c("harp_df", class(x))

  # The spatial class depends on the presence of geolist or xslist columns.
  # If neither are found assumed to be point data
  col_classes <- lapply(1:ncol(x), function(i) class(x[[i]]))
  if (any(sapply(col_classes, function(x) any(grepl("geolist", x))))) {
    classes <- c("harp_grid_df", classes)
  } else if (any(sapply(col_classes, function(x) any(grepl("xslist", x))))) {
    classes <- c("harp_xs_df", classes)
  } else {
    classes <- c("harp_point_df", classes)
  }

  # If the data frame is a forecast it will have either a mbr
  # or a det suffix. Note it could be lagged ensemble so the
  # mbr columns don't necessarily end the same way.

  col_names <- colnames(x)
  if (any(grepl("_det$", col_names))) {
    classes <- c(sub("harp", "harp_det", classes[1]), classes)
  }
  if (any(grepl("_mbr[[:digit:]]{3}", col_names))) {
    classes <- c(sub("harp", "harp_ens", classes[1]), classes)
  }

  structure(x, class = classes)

}

#' Create a list of harp data frames
#'
#' @param ... `harp_df` data frames. Must be named
#'
#' @return a `harp_list` list
#' @export
#'
#' @examples
#' as_harp_list(
#'   a = as_harp_df(data.frame(
#'     validdate = seq_dates(2021010100, 2021010123),
#'     a_det = runif(24)
#'   )),
#'   b = as_harp_df(data.frame(
#'     validdate = seq_dates(2021010100, 2021010123),
#'     b_det = runif(24)
#'   ))
#' )
#' as_harp_list(
#'   a = as_harp_df(data.frame(
#'     validdate = seq_dates(2021010100, 2021010123),
#'     a_mbr000  = runif(24),
#'     a_mbr001  = runif(24)
#'   )),
#'   b = as_harp_df(data.frame(
#'     validdate = seq_dates(2021010100, 2021010123),
#'     b_mbr000 = runif(24),
#'     b_mbr001 = runif(24)
#'   ))
#' )
as_harp_list <- function(...) {
  x <- list(...)
  # If ... is already a list, then only use the first entry as a new list was
  # made
  if (length(x) == 1 && depth(x) > 1 && !is.data.frame(x[[1]])) {
    x <- x[[1]]
  }
  if (is.null(names(x))) {
    stop("All ... must be named.")
  }
  if (!all(sapply(x, inherits, "harp_df"))) {
    stop("All ... must be `harp_df` data frames")
  }
  structure(x, class = c("harp_list", class(x)))
}

#' @export
print.harp_list <- function(x, ...) {
  .name <- names(x)
  print_fun <- function(.x, .y, ...) {
    cli::cat_bullet(.x, col = "#AAAAAA", bullet_col = "#AAAAAA")
    print(.y, ...)
    cat("\n")
  }
  invisible(mapply(print_fun, .name, x, MoreArgs = ...))
}

#' @export
c.harp_list <- function(x, ...) {
  as_harp_list(NextMethod())
}

#' @export
`[.harp_list` <- function(x, i, ...) {
  as_harp_list(NextMethod())
}




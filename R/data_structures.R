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
#' @param x a data frame with a `valid_dttm` column.
#'
#' @return a data frame with the appropriate class
#' @export
#'
#' @examples
#' library(dplyr)
#' d_f <- data.frame(valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)))
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
  has_valid_dttm <- is.element("valid_dttm", colnames(x))
  if (!has_valid_dttm) {
    stop("Data frame must have `valid_dttm` column.")
  }
  if (!inherits(x[["valid_dttm"]], "POSIXct")) {
    stop("`valid_dttm` column must be a date-time class (POSIXct).")
  }
  if (!tibble::is_tibble(x)) {
    x <- tibble::as_tibble(x)
  }
  # If already a harp_df, remove all harp classes and start again
  class(x) <- grep("^harp_", class(x), invert = TRUE, value = TRUE)
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
  # mbr columns don't necessarily end the same way. Gridded
  # analysis data frames have columns with the suffix anl

  col_names <- colnames(x)
  if (any(grepl("_det$", col_names))) {
    classes <- c(sub("harp", "harp_det", classes[1]), classes)
    x[["fcst_model"]] <- gsub(
      "_det$", "", grep("_det$", colnames(x), value = TRUE)
    )
    colnames(x) <- gsub("[[:graph:]]+_det$", "fcst", colnames(x))
    x <- dplyr::relocate(x, dplyr::all_of("fcst_model"))
  }
  if (any(grepl("_mbr[[:digit:]]{3}", col_names))) {
    classes <- c(sub("harp", "harp_ens", classes[1]), classes)
  }
  if (any(grepl("_anl$", col_names))) {
    classes <- c(sub("harp", "harp_anl", classes[1]), classes)
    x[["anl_model"]] <- gsub(
      "_anl$", "", grep("_anl$", colnames(x), value = TRUE)
    )
    colnames(x) <- gsub("[[:graph:]]+_anl$", "anl", colnames(x))
    x <- dplyr::relocate(x, dplyr::all_of("anl_model"))
  }

  structure(x, class = classes)

}

#' @rdname as_harp_df
#' @export
is_harp_df <- function(x) {
  inherits(x, "harp_df")
}

#' @export
print.harp_det_point_df <- function(x, ...) {
  cat(cli::col_green("::deterministic point forecast:: "))
  NextMethod()
}

#' @export
print.harp_det_grid_df <- function(x, ...) {
  cat(cli::col_green("::deterministic gridded forecast:: "))
  NextMethod()
}

#' @export
print.harp_det_xs_df <- function(x, ...) {
  cat(cli::col_green("::deterministic cross-section forecast:: "))
  NextMethod()
}

#' @export
print.harp_ens_point_df <- function(x, ...) {
  cat(cli::col_green("::ensemble point forecast:: "))
  NextMethod()
}

#' @export
print.harp_ens_grid_df <- function(x, ...) {
  cat(cli::col_green("::ensemble gridded forecast:: "))
  NextMethod()
}

#' @export
print.harp_ens_xs_df <- function(x, ...) {
  cat(cli::col_green("::ensemble cross-section forecast:: "))
  NextMethod()
}

#' @export
print.harp_anl_grid_df <- function(x, ...) {
  cat(cli::col_green("::gridded analysis:: "))
  NextMethod()
}

#' @export
print.harp_anl_xs_df <- function(x, ...) {
  cat(cli::col_green("::cross-section analysis:: "))
  NextMethod()
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
#'     valid_dttm = seq_dttm(2021010100, 2021010123),
#'     a_det = runif(24)
#'   )),
#'   b = as_harp_df(data.frame(
#'     valid_dttm = seq_dttm(2021010100, 2021010123),
#'     b_det = runif(24)
#'   ))
#' )
#' as_harp_list(
#'   a = as_harp_df(data.frame(
#'     valid_dttm = seq_dttm(2021010100, 2021010123),
#'     a_mbr000  = runif(24),
#'     a_mbr001  = runif(24)
#'   )),
#'   b = as_harp_df(data.frame(
#'     valid_dttm = seq_dttm(2021010100, 2021010123),
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

#' @rdname as_harp_list
#' @param x An object to test.
#' @export
is_harp_list <- function(x) {
  inherits(x, "harp_list")
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




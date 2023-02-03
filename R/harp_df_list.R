# Functions for manipulating data in or extracting data from
# harp_df data frames and harp_list lists.

#' Extract unique values from a data frame column
#'
#' Unique values are extracted from the named column in a data frame. In the
#' case of a \code{harp_list}, unique values across all of the data frames
#' in the list are extracted as a single vector. If a column is not found,
#' a warning is issued and NULL is returned.
#'
#' \code{unique_stations}, \code{unique_fcdate} and \code{unique_validdate}
#' are wrappers around \code{unique_col} that extract the unique stations,
#' valid date-time and forecast start date-time using the standard harp
#' column names "SID", "validdate" and "fcdate" respectively.
#'
#' @param .data A data frame or harp_list
#' @param col The column from which to extract the unique values. Can be quoted
#'   or unquoted.
#'
#' @return A vector of unique values.
#' @export
#'
#' @examples
unique_col <- function(.data, col) {
  UseMethod("unque_col")
}

#' @export
unique_col.data.frame <- function(.data, col) {
  col <- rlang::as_name(rlang::enquo(col))
  if (!is.element(col, colnames(.data))) {
    warning("Column: `", col, "` not found.")
    return(NULL)
  }
  col <- rlang::sym(col)
  unique(dplyr::pull(.data, !!col))
}

#' @export
unique_col.harp_list <- function(.data, col) {
  col <- rlang::sym(rlang::as_name(rlang::enquo(col)))
  unique(unlist(lapply(.data, unique_col, !!col)))
}

#' @rdname unique_col
#' @export
unique_stations <- function(.data) {
  unique_col(.data, "SID")
}

#' @rdname unique_col
#' @export
unique_validdate <- function(.data) {
  unique_col(.data, "validdate")
}

#' @rdname unique_col
#' @export
unique_fcdate <- function(.data) {
   unique_col(.data, "fcdate")
}

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
#' unique_col(det_point_df, fcdate)
#' unique_col(det_point_df, SID)
#'
#' # Works with quoted column names too
#' unique_col(det_point_df, "SID")
#'
#' # Use {{<var>}} for variables as columns
#' my_col <- "SID"
#' unique_col(det_point_df, {{my_col}})
unique_col <- function(.data, col) {
  UseMethod("unique_col")
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

#' Title
#'
#' @param .data
#'
#' @return
#' @export
#'
#' @examples
pivot_members <- function(.data) {
  UseMethod("pivot_members")
}

#' @export
pivot_members.harp_ens_point_df <- function(.data) {
  structure(
    pivot_to_long(.data),
    class = conditional_suffix(class(.data), "harp_", "_long")
  )
}

#' @export
pivot_members.harp_ens_grid_df <- function(.data) {
  structure(
    pivot_to_long(.data),
    class = conditional_suffix(class(.data), "harp_", "_long")
  )
}

#' @export
pivot_members.harp_ens_point_df_long <- function(.data) {
  structure(
    pivot_to_wide(.data),
    class = gsub("_long", "", class(.data))
  )
}

#' @export
pivot_members.harp_ens_grid_df_long <- function(.data) {
  structure(
    pivot_to_wide(.data),
    class = gsub("_long", "", class(.data))
  )
}

conditional_suffix <- function(x, regex, suffix) {
  ind <- grep(regex, x)
  x[ind] <- paste0(x[ind], suffix)
  x
}

pivot_to_long <- function(.data) {
  .data <- tidyr::pivot_longer(
    .data,
    dplyr::matches("_mbr[[:digit:]]{3}"),
    names_to  = "member",
    values_to = "forecast"
  )
  .data[["sub_model"]] <- regmatches(
    .data[["member"]],
    regexpr("^[[:graph:]]+(?=_mbr[[:digit:]]{3})", .data[["member"]], perl = TRUE)
  )
  .data[["member"]] <- regmatches(
    .data[["member"]],
    regexpr("(?<=_)mbr[[:digit:]]{3}[[:graph:]]*$", .data[["member"]], perl = TRUE)
  )
  if (is.element("fcst_model", colnames(.data))) {
    .data <- .data[c(
      "fcst_model", "sub_model",
      grep("fcst_model|sub_model", colnames(.data), value = TRUE, invert = TRUE)
    )]
  } else {
    .data <- .data[c(
      "sub_model",
      grep("fcst_model|sub_model", colnames(.data), value = TRUE, invert = TRUE)
    )]
  }
  .data
}

pivot_to_wide <- function(.data) {
  .data[["member"]] <- paste(
    .data[["sub_model"]], .data[["member"]], sep = "_"
  )
  .data <- .data[-grep("sub_model", colnames(.data))]
  tidyr::pivot_wider(
    .data, names_from = "member", values_from = "forecast"
  )
}

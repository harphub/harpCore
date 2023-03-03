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

#' Select ensemble members
#'
#' \code{select_members} is used to select specific ensemble members from a
#' data frame along with all other columns. The method can also be applied to
#' a \code{harp_list} of ensemble data frames.
#'
#' @param .data A data frame with ensemble members or a \code{harp_list}
#' @param members The members to select. Can be a numeric vector, or a named
#'   list to select members from specific forecast models in a \code{harp_list}
#'   object.
#' @param include_lagged Logical. Whether to include lagged ensemble members
#'   in the selection.
#'
#' @return A data frame with the selected members or a \code{harp_list} of
#'   data frames with the selected ensemble members.
#' @export
#'
#' @examples
#' select_members(ens_point_df, 0)
#' select_members(ens_grid_df, 1)
#'
#' # More than one member can be selected
#' select_members(ens_point_df, c(0, 1))
#'
#' # Select member 0 from a harp_list
#' select_members(ens_point_list, 0)
#'
#' # Different members can be selected from each data frame
#' select_members(ens_point_list, list(a = 0, b = 1))
select_members <- function(.data, members, include_lagged = TRUE) {
  UseMethod("select_members")
}

#' @export
select_members.harp_ens_point_df <- function(.data, members, include_lagged = TRUE) {
  member_select(.data, members, include_lagged)
}

#' @export
select_members.harp_ens_grid_df <- function(.data, members, include_lagged = TRUE) {
  member_select(.data, members, include_lagged)
}

#' @export
select_members.harp_list <- function(.data, members, include_lagged = TRUE) {

  if (is.list(members)) {

    if (is.null(names(members))) {

      if (length(members) == 1 && length(.data) > 1) {
        message(
          "Members only supplied for one forecast model.",
          "Recycling members for all forecast models."
        )
        members <- rep(members, length(.data))
      } else if (length(members) != length(.data)) {
        stop(
          paste(
            "Members supplied for", length(members), "forecast models",
            "when there are ", length(.data), "forecast models."
          ),
          call. = FALSE
        )
      } else {
        warning(
          "No forecast model names supplied for members. ",
          "Assuming they are in the correct order.",
          immediate. = TRUE, call. = FALSE
        )
        names(members) <- names(.data)
      }

    } else {
      bad_names <- setdiff(names(members), names(.data))
      if (length(bad_names) > 0) {
        stop(paste(bad_names, collapse = ", "), " not found in .data", call. = FALSE)
      }
    }

  } else {

    if (length(.data) > 1) {
      message(
        "Members only supplied for one forecast model. ",
        "Recycling members for all forecast models."
      )
    }
    members <- lapply(seq_along(.data), function(x) members)
    names(members) <- names(.data)
  }

  .data[names(members)] <- mapply(
    select_members, .data[names(members)], members,
    MoreArgs = list(include_lagged = include_lagged),
    SIMPLIFY = FALSE
  )
  .data

}

member_select <- function(df, members, lag_inc) {
  suffix    <- ifelse(lag_inc, "", "$")
  meta_cols <- grep("_mbr[[:digit:]]", colnames(df), invert = TRUE)
  data_cols <- lapply(
    members,
    function(x) {
      grep(
        paste0("_mbr", formatC(x, width = 3, flag = "0"), suffix),
        colnames(df)
      )
    }
  )
  data_cols <- unlist(data_cols[sapply(data_cols, length) != 0])
  dplyr::select(df, dplyr::all_of(c(meta_cols, data_cols)))
}



#' Pivot between wide and long ensemble data frames
#'
#' The default behaviour in harp is to store ensemble data in wide data frames.
#' That means that there is one column for each member of the ensemble. This
#' isn't always ideal and goes against the principles of tidy data, whereby
#' each ensemble member would be stored on a separate row with a single
#' column denoting the ensemble member. \code{pivot_members} can be used to
#' pivot between the wide and long formats in both directions.
#'
#' When pivoting from a wide to a long data frame, the class is updated to
#' indicate that the ensemble members are stored in rows rather than
#' columns. When pivoting back to a wide data frame format, the class is
#' returned to its original names.
#'
#' @param .data A harp data frame or a \code{harp_list} of data frames
#'
#' @return The same data frame, or harp_list but with the members pivoted.
#' @export
#'
#' @examples
#' pivot_members(ens_point_df)
#' pivot_members(ens_grid_df)
#' pivot_members(ens_point_list)
#' pivot_members(ens_grid_list)
#'
#' # Note the change in class
#' class(ens_point_df)
#' class(pivot_members(ens_point_df))
#' class(ens_grid_df)
#' class(pivot_members(ens_grid_df))
pivot_members <- function(.data) {
  UseMethod("pivot_members")
}

#' @export
pivot_members.harp_ens_point_df <- function(.data) {
  structure(
    pivot_to_long(.data),
    class = conditional_suffix(class(.data), "harp_ens", "_long")
  )
}

#' @export
pivot_members.harp_ens_grid_df <- function(.data) {
  structure(
    pivot_to_long(.data),
    class = conditional_suffix(class(.data), "harp_ens", "_long")
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

#' @export
pivot_members.harp_list <- function(.data) {
  as_harp_list(lapply(.data, pivot_members))
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

#' Expand date-time column in a data frame
#'
#' \code{expand_date} extracts the year, month, day, hour and minute from
#' a date-time column in a data frame and creates a column for each part
#' of the date. If the name of the date-time column ends with "date", the names
#' of the new columns are whatever precedes "date" in the original column name
#' followed by "_year", "_month", "_day", "_hour", "_minute". If the original
#' column name does not end with date the suffixes are pasted onto the
#' original column name.
#'
#' @param .data A data frame or harp_list of data frames
#' @param col The name of the date-time to column to be expanded. Can be quoted
#'   or unquoted. If using a variable, it should be wrapped in {{.}}
#' @param text_months Logical. If TRUE, month names are used rather than numbers.
#'
#' @return A data frame or harp_list of data frames with new columns for the
#'   expanded date
#' @export
#'
#' @examples
#' expand_date(det_point_df, fcdate)
#' expand_date(det_point_list, validdate)
#' expand_date(ens_point_df, validdate, text_months = TRUE)
#'
#' # Column name can be quoted
#' expand_date(ens_grid_df, "fcdate")
#'
#' # If using a variable, wrap in {{<var>}}
#' my_col <- "fcdate"
#' expand_date(ens_grid_df, {{my_col}})
expand_date <- function(.data, col, text_months = FALSE) {
  UseMethod("expand_date")
}

#' @export
expand_date.data.frame <- function(.data, col, text_months = FALSE) {
  col <- rlang::as_name(rlang::enquo(col))
  if (!is.element(col, colnames(.data))) {
    warning("Column: ", col, " not found.")
    return(invisible(.data))
  }
  dates <- .data[[col]]
  if (!inherits(dates, "POSIXct")) {
    warning("Column: ", col, " is not a date-time column.")
    return(invisible(.data))
  }
  prefix <- gsub("date$", "", col)
  .data[[paste0(prefix, "_year")]] <- as.integer(format(dates, "%Y"))
  if (text_months) {
    .data[[paste0(prefix, "_month")]] <- format(dates, "%b")
  } else {
    .data[[paste0(prefix, "_month")]] <- as.integer(format(dates, "%m"))
  }
  .data[[paste0(prefix, "_day")]] <- as.integer(format(dates, "%d"))
  .data[[paste0(prefix, "_hour")]] <- as.integer(format(dates, "%H"))
  .data[[paste0(prefix, "_minute")]] <- as.integer(format(dates, "%M"))
  .data
}

#' @export
expand_date.harp_list <- function(.data, col, text_months = FALSE) {
  col <- rlang::sym(rlang::as_name(rlang::enquo(col)))
  as_harp_list(lapply(.data, expand_date, !!col, text_months))
}



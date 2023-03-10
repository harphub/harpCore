# Functions for manipulating data in or extracting data from
# harp_df data frames and harp_list lists.

#' Extract unique values from a data frame column
#'
#' Unique values are extracted from the named column in a data frame. In the
#' case of a \code{harp_list}, unique values across all of the data frames
#' in the list are extracted as a single vector. If a column is not found,
#' a warning is issued and NULL is returned.
#'
#' \code{unique_stations}, \code{unique_fcst_dttm} and \code{unique_valid_dttm}
#' are wrappers around \code{unique_col} that extract the unique stations,
#' valid date-time and forecast start date-time using the standard harp
#' column names "SID", "valid_dttm" and "fcst_dttm" respectively.
#'
#' @param .data A data frame or harp_list
#' @param col The column from which to extract the unique values. Can be quoted
#'   or unquoted.
#'
#' @return A vector of unique values.
#' @export
#'
#' @examples
#' unique_col(det_point_df, fcst_dttm)
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
unique_valid_dttm <- function(.data) {
  unique_col(.data, "valid_dttm")
}

#' @rdname unique_col
#' @export
unique_fcst_dttm <- function(.data) {
   unique_col(.data, "fcst_dttm")
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
#' expand_date(det_point_df, fcst_dttm)
#' expand_date(det_point_list, valid_dttm)
#' expand_date(ens_point_df, valid_dttm, text_months = TRUE)
#'
#' # Column name can be quoted
#' expand_date(ens_grid_df, "fcst_dttm")
#'
#' # If using a variable, wrap in {{<var>}}
#' my_col <- "fcst_dttm"
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
  prefix <- gsub("_dttm$", "", col)
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

#' Join data to a forecast
#'
#' \code{join_to_fcst} is a special case of the join family of functions. It's
#' primary purpose is to join data frame of observations to a data frame or
#' harp_list of forecasts such that forecast - observation pairs are on the
#' same row in the joined data frame. An extra check is made to make sure that
#' the forecast data and observations data are in the same units.
#'
#' @param .fcst A \code{harp_df} data frame or a \code{harp_list}.
#' @param .join A data frame to join to the forecast.
#' @param join_type How to join the data frame. Acceptable values are: "inner",
#'   "left", "right", "full", "semi", "anti". See \code{\link[dplyr]{join}} for
#'   more details.
#' @param by Which columns to join by - if set to NULL a natural join will be
#'   done, using all variables with common names across .fcst and .join. The
#'   default is to join using all common columns in .fcst and .join excluding
#'   lat, lon and elev. This is because they may be stored to different levels of
#'   precision and the join will thus fail.
#' @param latlon Logical. Whether to include latitude and longitude columns in
#'   the default for \code{by}. The default is FALSE.
#' @param elev Logical. Whether to include the station elevation column in
#'   the default for \code{by}. The default is FALSE.
#' @param force Set to TRUE to force the join to happen even if the units
#'   in .fcst and .join are not compatible.
#' @param keep_x,keep_y Where duplicate column names are found, but not used in
#'   the join, these arguments are used to indicate whether the duplicate
#'   columns from .fcst (\code{keep_x}), or .join (\code{keep_y}) should be
#'   kept. The default is \code{keep_x = TRUE, keep_y = FALSE}.
#' @param ... Other arguments for \link[dplyr]{join}.
#'
#' @return The input forecast data frame with column(s) added from \code{.join}.
#' @export
#'
#' @examples
#' # Make some fake observations
#' library(tibble)
#' obs <- tibble(
#'   valid_dttm = det_point_df$valid_dttm,
#'   SID       = det_point_df$SID,
#'   units     = "degC",
#'   T2m       = runif(nrow(det_point_df))
#' )
#'
#' # Make sure the forecast has units
#' fcst <- set_units(det_point_df, "degC")
#'
#' join_to_fcst(fcst, obs)
#'
#' # Also works for harp_list objects
#' join_to_fcst(set_units(det_point_list, "degC"), obs)
#'
#' # And works with gridded data
#' join_to_fcst(set_units(ens_grid_df, "degC"), set_units(anl_grid_df, "degC"))
join_to_fcst <- function(
  .fcst,
  .join,
  join_type  = c("inner", "left", "right", "full", "semi", "anti"),
  by         = NULL,
  latlon     = FALSE,
  elev       = FALSE,
  force = FALSE,
  keep_x     = TRUE,
  keep_y     = FALSE,
  ...
) {
  UseMethod("join_to_fcst")
}

#' @export
join_to_fcst.harp_df <- function(
  .fcst,
  .join,
  join_type  = c("inner", "left", "right", "full", "semi", "anti"),
  by         = NULL,
  latlon     = FALSE,
  elev       = FALSE,
  force = FALSE,
  keep_x     = TRUE,
  keep_y     = FALSE,
  ...
) {

  join_type <- match.arg(join_type)

  join_func <- get(paste0(join_type, "_join"), envir = asNamespace("dplyr"))

  # Check for units columns
  has_fcst_units <- is.element("units", colnames(.fcst))
  has_join_units <- is.element("units", colnames(.join))

  if (has_fcst_units & has_join_units) {

    do_join <- TRUE

    fcst_units <- unique(.fcst$units)
    join_units <- unique(.join$units)

    if (length(fcst_units) != 1) {
      warning(".fcst has more than one units name: ", fcst_units, call. = FALSE, immediate. = TRUE)
      do_join <- FALSE
    } else if (length(unique(.fcst$units)) != 1) {
      warning(".join has more than one units name: ", join_units, call. = FALSE, immediate. = TRUE)
      do_join <- FALSE
    } else {
      if (fcst_units != join_units) {
        warning(".fcst has units: ", fcst_units, " and .join has units: ", join_units, call. = FALSE, immediate. = TRUE)
        do_join <- FALSE
      }
    }

  } else if (has_fcst_units & !has_join_units) {

    warning(".join does not have a units column. ", call. = FALSE, immediate. = TRUE)
    do_join <- FALSE

  } else if (!has_fcst_units & has_join_units) {

    warning(".fcst does not have a units column. ", call. = FALSE, immediate. = TRUE)
    do_join <- FALSE

  } else {

    warning("Neither .fcst nor .join have a units column.", call. = FALSE, immediate. = TRUE)
    do_join <- TRUE

  }

  if (is.null(by)) {
    by <- intersect(colnames(.fcst), colnames(.join))
    if (!latlon) {
      by <- by[!tolower(by) %in% c("lat", "lon", "latatitude", "longitude", "long")]
    }
    if (!elev) {
      by <- by[!tolower(by) %in% c("elev", "elevation", "altitude")]
    }
  }



  if (!do_join) {
    if (force) {
      message("Forcing join without units taken into account.")
      by <- by[by != "units"]
    } else {
      stop(
        "Join will not be done due to units incompatibility. ",
        "You can force the join by setting force = TRUE\n",
        "OR, units imcompatibility can be fixed with the set_units(), ",
        "or scale_param() functions.",
        call. = FALSE
      )
    }
  }

  by <- by[!by %in% c("lat", "lon", "elev")]
  message("Joining, by = c(\"", paste(by, collapse = "\", \""), "\")")

  .fcst <- suppressMessages(join_func(.fcst, .join, by = by, ...))

  if (!keep_x) {
    .fcst <- dplyr::select(.fcst, -dplyr::matches("\\.x$"))
  }

  if (!keep_y) {
    .fcst <- dplyr::select(.fcst, -dplyr::matches("\\.y$"))
  }

  if (!(keep_x && keep_y)) {
    .fcst <- dplyr::rename_with(.fcst, ~sub("\\.x$|\\.y$", "", .x))
  }

  .fcst

}

#' @export
join_to_fcst.harp_list <- function(
  .fcst,
  .join,
  join_type  = c("inner", "left", "right", "full", "semi", "anti"),
  by         = NULL,
  latlon     = FALSE,
  elev       = FALSE,
  force      = FALSE,
  keep_x     = TRUE,
  keep_y     = FALSE,
  ...
) {
  as_harp_list(lapply(
    .fcst, join_to_fcst, .join, join_type, by,
    latlon, elev,
    force, keep_x, keep_y, ...
  ))
}

#' Add or modify a units column
#'
#' @param x A data frame or a harp_list.
#' @param units The units name to put in the units column.
#'
#' @return A data frame or harp_list of the same size as x with the units
#'   column either added or modified.
#' @export
#'
#' @examples
#' # det_point_df has no units column
#' det_point_df
#'
#' # Set units to "degC"
#' new_det_point_df <- set_units(det_point_df, "degC")
#' new_det_point_df
#'
#' # Modify the units name to "degrees_C"
#' set_units(new_det_point_df, "degrees_C")
set_units <- function(x, units) {
  UseMethod("set_units")
}

#' @export
set_units.data.frame <- function(x, units) {
  x[["units"]] <- units

  regex <- "_mbr[[:digit:]]{3}|_det$"
  if (length(grep(regex, colnames(x))) > 0) {
    return(
      dplyr::relocate(
        x, "units", .before = dplyr::matches(regex)
      )
    )
  }
  x
}

#' @export
set_units.harp_list <- function(x, units) {
  as_harp_list(lapply(x, set_units, units))
}

#' Scale a parameter in a data frame
#'
#' @param x A data frame or a harp_list.
#' @param scaling The scaling to apply to the data. By default the scaling is
#'   additive, but if \code{mult = TRUE} it is multiplicative.
#' @param new_units The name of the new units. If missing, the units name will
#'   be unchanged.
#' @param mult Logical. Whether the scaling is multiplicative. The default is
#'   \code{FALSE}, meaning that the scaling is additive.
#' @param ... Used by methods.
#'
#' @return A data frame or \code{harp_list} with scaled parameter.
#' @export
#'
#' @examples
#' # Make a data frame of 2m temperature observations in degrees C
#' library(tibble)
#' obs <- tibble(
#'   valid_date = rep(seq_dttm(2022081500, 2022081523), 3),
#'   SID        = c(rep(1001, 24), rep(1002, 24), rep(1003, 24)),
#'   units      = "degC",
#'   T2m        = rnorm(24 * 3, 15, 2)
#' )
#'
#' # Scale to be in Kelvin
#' scale_param(obs, 273.15, "K", col = T2m)
#'
#' # col can be a quoted, or if a variable is must be wrapped in {{}}
#' scale_param(obs, 273.15, "K", col = "T2m")
#' prm <- "T2m"
#' scale_param(obs, 273.15, "K", col = {{prm}})
#'
#' # For forecast data frames, col is not needed
#' scale_param(det_point_df, 273.15, "K")
#' scale_param(ens_point_df, 273.15, "K")
#'
#' # Scaling can be multiplicative
#' scale_param(det_point_df, 100, "percent", mult = TRUE)
#' scale_param(ens_point_list, 1/1000, "kg/kg", mult = TRUE)
scale_param <- function(x, scaling, new_units, mult = FALSE, ...) {
  UseMethod("scale_param")
}

#' @param col The name of the column to scale - if \code{x} is a forecast or
#'   analysis \code{harp_df} data frame or a \code{harp_list}, the columns will
#'   be selected automatically and \code{col} is ignored.
#' @rdname scale_param
#' @export
scale_param.data.frame <- function(x, scaling, new_units, mult = FALSE, col, ...) {
  if (missing(col)) {
    stop("Don't know which column to scale. Set using col.")
  }
  col      <- rlang::enquo(col)
  col_name <- rlang::as_name(col)
  op  <- `+`
  if (mult) {
    op <- `*`
  }

  x[[col_name]] <- op(x[[col_name]], scaling)

  if (!missing(new_units)) {
    x[["units"]] <- new_units
  }

  dplyr::relocate(
    x,
    dplyr::any_of("units"),
    .before = !!col
  )
}

#' @export
scale_param.harp_df <- function(x, scaling, new_units, mult = FALSE, ...) {
  regex <- "_mbr[[:digit:]]{3}|_det$"

  op  <- `+`
  if (mult) {
    op <- `*`
  }
  x <- dplyr::mutate(x, dplyr::across(dplyr::matches(regex), ~op(.x, scaling)))

  if (!missing(new_units)) {
    x[["units"]] <- new_units
  }

  dplyr::relocate(
    x,
    dplyr::any_of("units"),
    .before = dplyr::matches(regex)
  )
}

#' @export
scale_param.harp_list <- function(x, scaling, new_units, mult = FALSE, ...) {
  as_harp_list(lapply(x, scale_param, scaling, new_units, mult, ...))
}

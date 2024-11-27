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
  unixtime_to_str_dttm(unique_col(.data, "valid_dttm"))
}

#' @rdname unique_col
#' @export
unique_fcst_dttm <- function(.data) {
  unixtime_to_str_dttm(unique_col(.data, "fcst_dttm"))
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
#' @param invert Logical. If `TRUE` all members \strong{except} those provided
#'   in `members` are returned.
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
#'
#' # Deselect members with invert = TRUE
#' select_members(ens_point_df, 0, invert = TRUE)
#' select_members(ens_point_list, list(a = 0, b = 1), invert = TRUE)
select_members <- function(
  .data, members, include_lagged = TRUE, invert = FALSE
) {
  UseMethod("select_members")
}

#' @export
select_members.harp_ens_point_df <- function(
    .data, members, include_lagged = TRUE, invert = FALSE
) {
  member_select(.data, members, include_lagged, invert)
}

#' @export
select_members.harp_ens_grid_df <- function(
  .data, members, include_lagged = TRUE, invert = FALSE
) {
  member_select(.data, members, include_lagged, invert)
}

#' @export
select_members.harp_list <- function(
  .data, members, include_lagged = TRUE, invert = FALSE
) {

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
    MoreArgs = list(include_lagged = include_lagged, invert = invert),
    SIMPLIFY = FALSE
  )
  .data

}

member_select <- function(df, members, lag_inc, invert) {
  suffix    <- ifelse(lag_inc, "", "$")
  meta_cols <- grep("_mbr[[:digit:]]", colnames(df), invert = TRUE)
  data_cols <- lapply(
    members,
    function(x) {
      grep(
        paste0("_mbr", formatC(x, width = 3, flag = "0"), suffix),
        colnames(df),
        invert = invert
      )
    }
  )
  if (invert) {
    data_cols <- Reduce(intersect, data_cols)
  } else {
    data_cols <- unlist(data_cols[sapply(data_cols, length) != 0])
  }
  dplyr::select(df, dplyr::all_of(union(meta_cols, data_cols)))
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
    names_to  = c("sub_model", "member"),
    values_to = "fcst",
    names_sep = "_(?=mbr[[:digit:]]{3})"
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
  tidyr::pivot_wider(
    .data, names_from = dplyr::any_of(c("sub_model", "member")),
    values_from = "fcst", names_sep = "_"
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

#' Join multiple groups to a point forecast data frame or `harp_list`
#'
#' `join_multi_groups` adds a column to a `harp_det_point_df` or a
#' `harp_ens_point_df` data frame to where each row can be in mulitple groups.
#' This column can then be used as a grouping column in verification
#' functions. By default, the built in station group data `station_groups` is
#' used, but any data frame with a common column with the forecast data that
#' uniquely identifies a row can be used. An attribute is added so that the
#' verification functions know to treat this column as a multi-group column. All
#' rows in the output acquire an "All" value, meaning that rows that aren't
#' included in the grouping data frame are given the value "All" in the output.
#'
#' @param group_df a data frame with a column that is common to `.fcst` that
#'   uniquely identifies a row, and a column for the group name.
#' @param group_col <[`tidy-select`][dplyr::dplyr_tidy_select]> The name of the
#'   column in `group_df` that contains the name of the station group.
#'
#' @return An object of the same class as `.fcst` with a multi-group column.
#' @export
#'
#' @examples
#' join_multi_groups(det_point_df)
#'
#' # Note that each grouping is enclosed in < >
#' #
#' # Use custom groups
#' grps <- data.frame(
#'   SID = c(1001, 1001, 1002, 1002, 1002),
#'   letter = c("A", "B", "A", "C", "D")
#' )
#' join_multi_groups(det_point_df, grps, letter)
#'
#' # Where a station is not in a group it gets the value "All" (note also the
#' # use of tidy selection)
#' ll <- "letter"
#' grps <- data.frame(
#'   SID = 1002,
#'   letter = "A"
#' )
#' join_multi_groups(det_point_df, grps, {{ll}})
join_multi_groups <- function(
  .fcst,
  group_df  = getExportedValue("harpCore", "station_groups"),
  group_col = "station_group"
) {
  UseMethod("join_multi_groups")
}

#' @export
join_multi_groups.data.frame <- function(
  .fcst,
  group_df  = getExportedValue("harpCore", "station_groups"),
  group_col = "station_group"
) {

  group_col <- rlang::ensym(group_col)
  group_col_name <- rlang::as_name(group_col)
  group_vals <- union(
    "All",
    unique(group_df[[group_col]][group_df[["SID"]] %in% .fcst[["SID"]]])
  )

  if (!is.list(dplyr::pull(group_df, !!group_col))) {
    group_df <- dplyr::summarise(
      group_df,
      !!group_col := paste0(
        "<", paste(union("All", unique(!!group_col)), collapse = "><"), ">"
      ),
      .by = "SID"
    )
  }

  .fcst <- suppressMessages(suppressWarnings(
    join_to_fcst(.fcst, group_df, "left", force = TRUE)
  ))

  .fcst <- dplyr::mutate(
    .fcst,
    !!group_col := dplyr::case_when(
      is.na(!!group_col) ~ "<All>",
      .default = !!group_col
    )
  )

  attr(.fcst, "multi_groups") <- list()
  attr(.fcst, "multi_groups")[[group_col]] <- paste0("<", group_vals, ">")

  .fcst

}

#' @export
join_multi_groups.harp_list <- function(
  .fcst,
  group_df  = getExportedValue("harpCore", "station_groups"),
  group_col = "station_group"
) {
  as_harp_list(lapply(
    .fcst,
    function(x) join_multi_groups(x, group_df, {{group_col}})
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
  regex <- "_mbr[[:digit:]]{3}|_det$|^fcst$|^forecast$|^anl$|^analysis$"

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


#' Decumulate accumulated variables
#'
#' Many models store accumulated values as the amount accumulated since the
#' start of the forecast. `decum()` decumulates those values to specific time
#' periods. Note that the time periods are overlapping, such that for hourly
#' data a (for example) 6-hour accumulation will be calculated every hour for
#' the previous 6 hours rather than in consecutive 6-hour windows.
#'
#' @param .data A `harp_df` data fram or `harp_list`
#' @param decum_time The time period to which to decumulate the data. If
#'   numeric, it is considered to be in hours, otherwise a string with a number
#'   followed by a unit. Units can be "s", for seconds; "m", for minutes; "h",
#'   for hours; or "d", for days.
#' @param cols Columns for which to do the decumulation. Uses the same semantics
#'   as \code{\link[dplyr]{select}}.
#' @param time_col The time column to use for calculating decumulation windows.
#'   Uses the same semantics as \code{\link[dplyr]{select}}.
#' @param group_col The columns to group the output by. Uses the same semantics
#'   as \code{\link[dplyr]{select}}.
#' @param df_name The name of the data frame - only really used to generate
#'   error messages.
#' @param ... Used for methods.
#'
#' @return An object of the same class as `.data` containing the decumulated
#'   values.
#' @export
decum <- function(.data, decum_time, cols, time_col, group_col, df_name = NULL, ...) {
  UseMethod("decum")
}

# Methods set default args, but can still be changed
#' @export
decum.harp_det_point_df <- function(
  .data,
  decum_time,
  cols      = dplyr::matches("_det$|^forecast$|^fcst$"),
  time_col  = "lead_time",
  group_col = c("fcst_dttm", "SID"),
  df_name   = NULL,
  ...
) {
  .data <- decum_df(
    .data, decum_time, {{cols}}, {{time_col}}, {{group_col}}, df_name
  )
  as_harp_df(
    dplyr::filter(
      .data,
      dplyr::if_any({{cols}}, ~ !is.na(.x))
    )
  )
}


#' @export
decum.harp_det_grid_df <- function(
  .data,
  decum_time,
  cols      = dplyr::matches("_det$|^forecast$|^fcst$"),
  time_col  = "lead_time",
  group_col = "fcst_dttm",
  df_name   = NULL,
  ...
) {
  .data <- decum_df(
    .data, decum_time, {{cols}}, {{time_col}}, {{group_col}}, df_name
  )
  as_harp_df(
    dplyr::filter(
      .data,
      dplyr::if_any({{cols}}, ~ !vapply(.x, is.null, TRUE))
    )
  )
}

#' @export
decum.harp_ens_point_df <- function(
  .data,
  decum_time,
  cols      = dplyr::matches("_mbr[[:digit:]]+"),
  time_col  = "lead_time",
  group_col = c("fcst_dttm", "SID"),
  df_name   = NULL,
  ...
) {
  .data <- decum_df(
    .data, decum_time, {{cols}}, {{time_col}}, {{group_col}}, df_name
  )
  as_harp_df(
    dplyr::filter(
      .data,
      dplyr::if_any({{cols}}, ~ !is.na(.x))
    )
  )
}

#' @export
decum.harp_ens_grid_df <- function(
  .data,
  decum_time,
  cols      = dplyr::matches("_mbr[[:digit:]]+"),
  time_col  = "lead_time",
  group_col = "fcst_dttm",
  df_name   = NULL,
  ...
) {
  .data <- decum_df(
    .data, decum_time, {{cols}}, {{time_col}}, {{group_col}}, df_name
  )
  as_harp_df(
    dplyr::filter(
      .data,
      dplyr::if_any({{cols}}, ~ !vapply(.x, is.null, TRUE))
    )
  )
}

#' @export
decum.harp_anl_grid_df <- function(
  .data,
  decum_time,
  cols        = dplyr::matches("_anl$|^anl$|^analysis$"),
  time_col    = "valid_dttm",
  group_col   = NULL,
  df_name     = NULL,
  accum_first = TRUE,
  ...
) {
  # For an analysis, it is more likely that the data are already decumulated,
  # in which case, everything is accumulated first to make it possible to
  # get any decumulation time.

  if (accum_first) {
    .data <- dplyr::mutate(
      dplyr::arrange(.data, {{time_col}}),
      dplyr::across({{cols}}, cumsum)
    )
  }

  .data <- decum_df(
    .data, decum_time, {{cols}}, {{time_col}}, {{group_col}}, df_name
  )
  as_harp_df(
    dplyr::filter(
      .data,
      dplyr::if_any({{cols}}, ~ !vapply(.x, is.null, TRUE))
    )
  )
}

#' @export
decum.data.frame <- function(
  .data,
  decum_time,
  cols      = dplyr::matches("_mbr[[:digit:]]+"),
  time_col  = "lead_time",
  group_col = c("fcst_dttm", "SID"),
  df_name   = NULL,
  ...
) {
  .data <- decum_df(
    .data, decum_time, {{cols}}, {{time_col}}, {{group_col}}, df_name
  )
  dplyr::filter(
    .data,
    dplyr::if_any(
      {{cols}},
      ~if (is.list(.x)) {
        vapply(.x, length, numeric(1)) > 0
      } else {
        !is.na(.x)
      }
    )
  )
}


#' @export
decum.harp_list <- function(
  .data,
  decum_time,
  ...
) {
  as_harp_list(
    mapply(
      decum,
      .data    = .data,
      df_name  = names(.data),
      MoreArgs = list(decum_time = decum_time, ...),
      SIMPLIFY = FALSE
    )
  )
}


# This function does the work
decum_df <- function(
    .data, decum_time, cols = NULL, time_col, group_col, df_name = NULL
) {

  if (rlang::quo_is_null(rlang::enquo(cols))) {
    cols <- grep(
      "_mbr[[:digit:]]+|_det$|_anl$|^forecast$|^fcst$", colnames(.data), value = TRUE
    )
    if (length(cols) < 1) {
      stop("No appropriate columns found to decumulate.", call. = FALSE)
    }
  }

  decum_time <- to_seconds(decum_time)

  # Check time_col and add empty times to make everything the same time
  # resolution if time resolution is not consistent throughout
  data_times <- dplyr::pull(.data, {{time_col}})
  if (inherits(data_times, "POSIXct")) {
    .data[["data_times"]] <- as.numeric(data_times)
  } else {
    .data[["data_times"]] <- to_seconds(data_times)
  }
  data_times <- sort(unique(.data[["data_times"]]))
  max_dt     <- ceiling(max(data_times) / decum_time) * decum_time
  dt_res     <- unique(diff(data_times))

  if (length(dt_res) > 1) {
    data_times <- seq(min(data_times), max_dt, min(dt_res))
    if (rlang::quo_is_null(rlang::enquo(group_col))) {
      df_df <- tibble::tibble(data_times = data_times)
    } else {
      dt_df <- dplyr::mutate(
        dplyr::distinct(dplyr::select(.data, {{group_col}})),
        data_times = list(data_times)
      ) %>%
        tidyr::unnest(dplyr::all_of("data_times"))
    }
    .data <- dplyr::full_join(.data, dt_df)
  }

  dt_res <- unique(diff(sort(unique(.data[["data_times"]]))))
  if (length(dt_res) > 1) {
    stop("Something isn't right with the data times.", call. = FALSE)
  }
  if (decum_time %% dt_res != 0) {
    warning(
      df_name,
      ": Interval between lead times is not usable for decum_time = ",
      names(decum_time),
      "\nNo decumulation done.",
      call.      = FALSE,
      immediate. = TRUE
    )
    return(.data[grep("data_times", colnames(.data), invert = TRUE)])
  }
  lag_rows <- decum_time / dt_res

  # Group the data by fcdate, order by leadtime and compute the difference
  decum_func <- function(df, cols, lag) {
    dplyr::mutate(
      dplyr::arrange(df, .data[["data_times"]]),
      dplyr::across({{cols}}, ~.x - dplyr::lag(.x, lag))
    )
  }

  dplyr::group_by(.data, dplyr::across({{group_col}})) %>%
    decum_func({{cols}}, lag_rows) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(dplyr::across({{group_col}})) %>%
    dplyr::select(-dplyr::all_of("data_times"))
}


#' Compute basic ensemble statistics
#'
#' Given a data frame of ensemble forecasts the ensemble mean, the ensemble
#' standard deviation (spread), ensemble variance, ensemble minimum, and
#' ensemble are calculated.
#'
#' By default only the ensemble mean and standard deviation are computed. Note
#' that the ensemble median is not yet implemented for gridded data.
#'
#' @param .fcst A `harp_ens_grid_df` or `harp_ens_point_df` data frame, or a
#'   `harp_list` containing data frames with those classes.
#' @param mean Logical. Whether to compute the ensemble mean.
#' @param sd Logical. Whether to compute the ensemble standard deviation.
#' @param var Logical. Whether to compute the ensemble variance.
#' @param min Logical. Whether to compute the ensemble minumum.
#' @param max Logical. Whether to compute the ensemble maximum.
#' @param keep_members Logical. Whether to keep the ensemble members in the
#'   returned object. The default is to only return the statistics.
#' @param ... Not used.
#'
#' @return An object of the same class as `.fcst` with columns for the ensemble
#'   statistics
#' @export
#'
#' @examples
#' ens_stats(ens_point_df)
#' ens_stats(ens_point_df, keep_members = TRUE)
#' ens_stats(ens_point_df, var = TRUE, min = TRUE, max = TRUE)
#' ens_stats(ens_grid_list)
ens_stats <- function(
  .fcst,
  mean         = TRUE,
  sd           = TRUE,
  var          = FALSE,
  min          = FALSE,
  max          = FALSE,
  keep_members = FALSE,
  ...
) {
  UseMethod("ens_stats")
}

#' @export
ens_stats.harp_ens_grid_df <- function(
  .fcst,
  mean         = TRUE,
  sd           = TRUE,
  var          = FALSE,
  min          = FALSE,
  max          = FALSE,
  keep_members = FALSE,
  ...
) {

  stats <- c(mean, sd, var, min, max)
  names(stats) <- c("mean", "sd", "var", "min", "max")

  if (!any(stats)) {
    return(.fcst)
  }

  saved_class <- class(.fcst)

  res <- pivot_members(.fcst)

  group_cols <- colnames(res)[!colnames(res) %in% c("member", "fcst")]

  res <- dplyr::group_by(res, !!!rlang::syms(group_cols))

  res <- dplyr::summarise(
    res,
    ens_mean = if (stats[["mean"]]) mean(.data[["fcst"]]) else NA,
    ens_sd   = if (stats[["sd"]]) std_dev(.data[["fcst"]]) else NA,
    ens_var  = if (stats[["var"]]) variance(.data[["fcst"]]) else NA,
    ens_min  = if (stats[["min"]]) min(.data[["fcst"]]) else NA,
    ens_max  = if (stats[["max"]]) max(.data[["fcst"]]) else NA,
    .groups = "drop"
  )

  res <- dplyr::select(
    res,
    dplyr::all_of(group_cols),
    dplyr::all_of(paste0("ens_", names(stats))[stats])
  )

  if (length(unique(res[["sub_model"]])) < 2) {
    res <- dplyr::select(res, -dplyr::all_of("sub_model"))
  }

  if (keep_members) {
    return(suppressMessages(dplyr::inner_join(.fcst, res)))
  }

  structure(
    res,
    class = grep("harp_ens_grid_df", saved_class, value = TRUE, invert = TRUE)
  )

}

#' @rdname ens_stats
#' @param median Logical. Whether to compute the ensemble median.
#' @export
ens_stats.harp_ens_point_df <- function(
    .fcst,
    mean         = TRUE,
    sd           = TRUE,
    var          = FALSE,
    min          = FALSE,
    max          = FALSE,
    keep_members = FALSE,
    median       = FALSE,
  ...
) {

  stats <- c(mean, sd, var, min, max, median)
  names(stats) <- c("mean", "sd", "var", "min", "max", "median")

  if (!any(stats)) {
    return(.fcst)
  }

  all_cols    <- colnames(.fcst)
  member_cols <- grep("_mbr[[:digit:]]{3}", all_cols)

  res <- dplyr::mutate(
    .fcst,
    ens_mean = if (stats[["mean"]]) rowMeans(dplyr::pick(member_cols)) else NA,
    ens_sd   = if (stats[["sd"]]) {
      matrixStats::rowSds(as.matrix(dplyr::pick(member_cols)))
    } else {
      NA
    },
    ens_var = if (stats[["var"]]) {
      matrixStats::rowVars(as.matrix(dplyr::pick(member_cols)))
    } else {
      NA
    },
    ens_min = if (stats[["min"]]) {
      matrixStats::rowMins(as.matrix(dplyr::pick(member_cols)))
    } else {
      NA
    },
    ens_max = if (stats[["max"]]) {
      matrixStats::rowMaxs(as.matrix(dplyr::pick(member_cols)))
    } else {
      NA
    },
    ens_median = if (stats[["median"]]) {
      matrixStats::rowMedians(as.matrix(dplyr::pick(member_cols)))
    } else {
      NA
    }
  )

  res <- dplyr::select(
    res,
    dplyr::all_of(all_cols),
    dplyr::all_of(paste0("ens_", names(stats))[stats])
  )

  if (keep_members) {
    return(res)
  }

  res <- res[-member_cols]

  structure(
    res,
    class = grep("harp_ens_point_df", class(res), value = TRUE, invert = TRUE)
  )

}

#' @export
ens_stats.harp_list <- function(
  .fcst,
  mean         = TRUE,
  sd           = TRUE,
  var          = FALSE,
  min          = FALSE,
  max          = FALSE,
  keep_members = FALSE,
  median       = FALSE,
  ...
) {
  as_harp_list(
    lapply(
      .fcst,
      ens_stats,
      mean         = mean,
      sd           = sd,
      var          = var,
      min          = min,
      max          = max,
      median       = median,
      keep_members = keep_members,
      ...
    )
  )
}

#' Compute the ensemble probability for a threshold
#'
#' The probability for a threshold for an ensemble is computed. This is by
#' default for threshold exceedence (P(fcst >= threshold)), but the probability
#' for being below the threshold, or between or outside of two thresholds can
#' also be calculated.
#'
#' Note that when a `geolist` is passed to the function, each element of the
#' `geolist` is assumed to be an ensemble member.
#'
#' @param x A `harp_ens_grid_df` or `harp_ens_point_df` data frame, a `geolist`,
#'   or a `harp_list` containing ensemble data frames.
#' @inheritParams nbhd_smooth
#' @param ... Used for methods.
#' @return An object of the same class as `x` with the probabilities computed
#' @export
#'
#' @examples
#' p_ge_0.5 <- ens_prob(ens_grid_df, 0.5)
#' image(p_ge_0.5$prob_ge_0.5[[1]])
#'
#' p_le_0.1 <- ens_prob(ens_grid_df, 0.1, comparator = "le")
#' image(p_le_0.1$prob_le_0.1[[1]])
#'
#' p_btw_0.25_0.75 <- ens_prob(
#'   ens_grid_df, c(0.25, 0.75), comparator = "between"
#' )
#' image(p_btw_0.25_0.75$prob_between_0.25_0.75[[1]])
ens_prob <- function(
  x,
  threshold    = 0,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  ...
) {
  UseMethod("ens_prob")
}

#' @export
ens_prob.harp_geolist <- function(
    x,
  threshold    = 0,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  ...
) {
  mean(nbhd_smooth(x, 0, threshold, comparator, include_low, include_high))
}



#' @inheritParams geo_transform
#' @export
ens_prob.harp_ens_grid_df <- function(
  x,
  threshold    = 0,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  keep_members = FALSE,
  ...
) {

  comparator <- match.arg(comparator)

  saved_class <- class(x)

  res <- pivot_members(x)

  group_cols <- colnames(res)[!colnames(res) %in% c("member", "fcst")]

  res <- dplyr::summarise(
    res,
    prob = ens_prob(
      .data[["fcst"]], threshold, comparator, include_low, include_high
    ),
    .by = dplyr::all_of(group_cols)
  )

  colnames(res)[colnames(res) == "prob"] <- paste(
    "prob",
    comparator,
    paste(threshold,  collapse = "_"),
    sep = "_"
  )

  if (length(unique(res[["sub_model"]])) < 2) {
    res <- dplyr::select(res, -dplyr::all_of("sub_model"))
  }

  if (keep_members) {
    return(suppressMessages(dplyr::inner_join(x, res)))
  }

  structure(
    res,
    class = grep("harp_ens_grid_df", saved_class, value = TRUE, invert = TRUE)
  )

}

#' @export
ens_prob.harp_ens_point_df <- function(
    x,
    threshold    = 0,
    comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
    include_low  = TRUE,
    include_high = TRUE,
    keep_members = FALSE,
    ...
) {

  comparator <- match.arg(comparator)

  saved_class <- class(x)

  member_cols <- grep("_mbr[[:digit:]]{3}", colnames(x))

  comp_func <- switch(
    comparator,
    "ge"      = function(.x) `>=`(.x, threshold),
    "gt"      = function(.x) `>`(.x, threshold),
    "le"      = function(.x) `<=`(.x, threshold),
    "lt"      = function(.x) `<`(.x, threshold),
    "between" = if (include_low && include_high) {
      function(.x) .x >= min(threshold) && .x <= max(threshold)
    } else if (include_low && !include_high) {
      function(.x) .x >= min(threshold) && .x < max(threshold)
    } else if (!include_low && include_high) {
      function(.x) .x > min(threshold) && .x <= max(threshold)
    } else {
      function(.x) .x > min(threshold) && .x < max(threshold)
    },
    "outside" = if (include_low && include_high) {
      function(.x) .x <= min(threshold) || .x >= max(threshold)
    } else if (include_low && !include_high) {
      function(.x) .x <= min(threshold) || .x > max(threshold)
    } else if (!include_low && include_high) {
      function(.x) .x < min(threshold) || .x >= max(threshold)
    } else {
      function(.x) .x < min(threshold) || .x > max(threshold)
    }
  )

  res <- dplyr::mutate(
    x,
    dplyr::across(dplyr::all_of(member_cols), comp_func)
  )

  res <- ens_stats(res, sd = FALSE)

  colnames(res)[colnames(res) == "ens_mean"] <- paste(
    "prob", comparator, threshold, sep = "_"
  )

  if (keep_members) {
    return(suppressMessages(dplyr::inner_join(x, res)))
  }

  res

}

#' @export
ens_prob.harp_list <- function(
  x,
  threshold    = 0,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  keep_members = FALSE,
  ...
) {

  comparator <- match.arg(comparator)

  as_harp_list(
    lapply(
      x,
      ens_prob,
      threshold    = threshold,
      comparator   = comparator,
      include_low  = include_low,
      include_high = include_high,
      keep_members = keep_members,
      ...
    )
  )
}

#' Compute the ensemble mean and variance
#'
#' `r lifecycle::badge("superseded")` This function is superseded by
#' \code{\link[harpCore]{ens_stats}}. However, `ens_mean_and_var()` is still
#' useful for computing the ensemble spread with a dropped member and is used by
#' \code{\link[harpPoint]{ens_spread_and_skill}}.
#'
#' The ensemble mean and variance are computed and added as columns to tables in
#' a \code{harp_df} object or `harp_list` object..
#'
#' @param .fcst A \code{harp_list} object, or a `harp_ens_point_df` or
#'   `harp_ens_grid_df` data frame.
#' @param mean_name The output column name for the ensemble mean
#' @param var_name The output column name for the ensemble variance
#' @param sd_name The output column name for the ensemble spread (standard
#'   deviation)
#' @param var_drop_member Which members to drop for the calculation of the
#'   ensemble variance and standard deviation. For harp_fcst objects, this can
#'   be a numeric scalar - in which case it is recycled for all forecast models;
#'   a list or numeric vector of the same length as the harp_fcst object, or a
#'   named list with the names corresponding to names in the harp_fcst object.
#'
#' @return A \code{harp_fcst} object with columns ens_mean and ens_var added to
#'   the forecast tables.
#' @export
#'
ens_mean_and_var <- function(
    .fcst, mean_name = "ens_mean", var_name = "ens_var", sd_name = "ens_spread",
  var_drop_member = NULL
) {
  lifecycle::deprecate_soft(
    "0.1.0",
    "ens_mean_and_var()",
    "ens_stats()",
    c("i" = paste(
      "If you need to use `var_drop_member` continue to use",
      "`ens_mean_and_var()`."
    ))
  )
  UseMethod("ens_mean_and_var")
}

#' @export
ens_mean_and_var.harp_ens_point_df <- function(
    .fcst, mean_name = "ens_mean", var_name = "ens_var",
  sd_name = "ens_spread", var_drop_member = NULL
) {
  col_names <- colnames(.fcst)
  mean_name <- rlang::sym(mean_name)
  var_name  <- rlang::sym(var_name)
  sd_name   <- rlang::sym(sd_name)

  if (length(grep("_mbr", col_names)) < 1) {
    stop(".fcst column names must contain '_mbr' to indicate an ensemble", call. = FALSE)
  }

  drop_members <- "TheRegexShouldNeverExistInnit"

  if (!is.null(var_drop_member)) {
    if (!is.numeric(var_drop_member)) {
      stop("`var_drop_member` must be numeric.", call. = FALSE)
    }
    drop_members <- paste(
      paste0("_mbr", formatC(var_drop_member, width = 3, flag = "0"), "$"),
      sep = "|"
    )

  }

  member_data <- dplyr::select(.fcst, dplyr::contains("_mbr"))

  .fcst <- dplyr::mutate(
    .fcst,
    !!mean_name := rowMeans(member_data),
    !!var_name  := matrixStats::rowVars(as.matrix(member_data)),
    !!sd_name   := sqrt(!!var_name)
  )

  if (!is.null(var_drop_member)) {
    dm_var_name <- rlang::sym(paste0("dropped_members_", var_name))
    dm_sd_name <- rlang::sym(paste0("dropped_members_", sd_name))
    .fcst <- dplyr::mutate(
      .fcst,
      !!dm_var_name := matrixStats::rowVars(
        as.matrix(dplyr::select(member_data, -dplyr::matches(drop_members)))
      ),
      !!dm_sd_name  := sqrt(!!dm_var_name)
    )
  }

  .fcst

}

#' @export
ens_mean_and_var.harp_ens_grid_df <- function(
    .fcst, mean_name = "ens_mean", var_name = "ens_var",
  sd_name = "ens_spread", var_drop_member = NULL
) {
  col_names <- colnames(.fcst)
  mean_name <- rlang::sym(mean_name)
  var_name  <- rlang::sym(var_name)
  sd_name   <- rlang::sym(sd_name)

  if (length(grep("_mbr", col_names)) < 1) {
    stop(".fcst column names must contain '_mbr' to indicate an ensemble", call. = FALSE)
  }

  member_data <- lapply(
    purrr::transpose(dplyr::select(.fcst, dplyr::contains("_mbr"))),
    geolist
  )

  .fcst <- dplyr::mutate(
    .fcst,
    !!mean_name := do.call("c", lapply(member_data, mean)),
    !!var_name  := do.call("c", lapply(member_data, variance)),
    !!sd_name   := sqrt(!!var_name)
  )

  if (!is.null(var_drop_member)) {

    drop_members <- "TheRegexShouldNeverExistInnit"

    if (!is.numeric(var_drop_member)) {
      stop("`var_drop_member` must be numeric.", call. = FALSE)
    }
    drop_members <- paste(
      paste0("_mbr", formatC(var_drop_member, width = 3, flag = "0"), "$"),
      sep = "|"
    )

    member_data <- lapply(
      purrr::transpose(
        dplyr::select(
          .fcst, dplyr::contains("_mbr"), -dplyr::matches(drop_members)
        )
      ),
      geolist
    )

    dm_var_name <- rlang::sym(paste0("dropped_members_", var_name))
    dm_sd_name <- rlang::sym(paste0("dropped_members_", sd_name))
    .fcst <- dplyr::mutate(
      .fcst,
      !!dm_var_name := matrixStats::rowVars(
        as.matrix(dplyr::select(member_data, -dplyr::matches(drop_members)))
      ),
      !!dm_sd_name  := sqrt(!!dm_var_name)
    )
  }

  .fcst

}

#' @export
ens_mean_and_var.harp_list <- function(
    .fcst, mean_name = "ens_mean", var_name = "ens_var",
  sd_name = "ens_spread", var_drop_member = NULL
) {

  var_drop_member <- parse_member_drop(var_drop_member, names(.fcst))

  structure(
    purrr::map2(
      .fcst, var_drop_member,
      ~ens_mean_and_var(.x, mean_name, var_name, sd_name, var_drop_member = .y)
    ),
    class = "harp_fcst"
  )
}

parse_member_drop <- function(x, nm) {

  if (!is.null(names(x))) {
    x <- as.list(x)
  }

  if (!is.list(x)) {
    if (is.null(x)) {
      return(sapply(nm, function(x) NULL, simplify = FALSE))
    }
    if (length(x) == 1) {
      return(sapply(nm, function(.x) x, simplify = FALSE))
    }
    if (length(x) == length(nm)) {
      x <- as.list(x)
      names(x) <- nm
      return(x)
    }
    stop("Bad input for `spread_exclude_member`", call. = FALSE)
  }

  if (is.null(names(x))) {

    if (length(x) == length(nm)) {
      names(x) <- nm
      return(x)
    }

    stop(
      "If `spread_exclude_member` is a list ",
      "it must be the same length as `.fcst` or have names",
      call. = FALSE
    )

  }

  if (identical(sort(names(x)), sort(nm))) {
    return(x[nm])
  }

  if (length(intersect(names(x), nm)) < 1) {
    stop(
      "spread_exclude_member: ",
      paste(names(x), collapse = ", "),
      " not found in `.fcst`.",
      call. = FALSE
    )
  }

  if (length(setdiff(names(x), nm)) > 0) {
    stop(
      "spread_exclude_member: ",
      paste(setdiff(names(x), nm), collapse = ", "),
      " not found in `.fcst`.",
      call. = FALSE
    )
  }

  x <- c(x, sapply(setdiff(nm, names(x)), function(x) NULL, simplify = FALSE))

  x[nm]

}


#' Filter to common cases
#'
#' For a fair comparison of models, the verification should only be done for
#' dates and locations that are common to all models. \code{common_cases} takes
#' a harp_list object as input and then identifies and filters to only those
#' cases that are common to all of the forecast models in the harp_list object.
#' By default this is done with the SID, fcst_dttm and lead_time columns, but
#' extra columns can be added via `...`. If one of the columns is a vertical
#' coordinate ("p", "z", "ml" for pressure, height and model level
#' respectively), that column will also be included.
#'
#' @param .fcst A harp_list object
#' @param ... Extra columns from which to determine the common cases. To remove
#'   one of the default columns from the test use `-<col>`.
#' @param rows_only Logical. Default is `FALSE`. Set to `TRUE` to only return
#'   a data frame of the meta data for the common cases. That is to say, a
#'   data frame with columns for SID, fcst_dttm and lead_time as well as any
#'   columns specified in `...`
#'
#' @return The input data frame with only the common stations and forecast dates
#'   for each forecast model selected.
#' @export
common_cases <- function(.fcst, ..., rows_only = FALSE) {
  UseMethod("common_cases")
}

#' @export
common_cases.harp_df <- function(.fcst, ..., rows_only = FALSE) {
  .fcst
}

#' @export
common_cases.harp_list <- function(.fcst, ..., rows_only = FALSE) {

  if (length(.fcst) < 2) {
    return(.fcst)
  }

  common_cols <- c("SID", "fcst_dttm", "lead_time", "p", "z", "ml")
  named_cols  <- vapply(rlang::enquos(...), rlang::as_label, character(1))
  named_cols  <- gsub("\"", "", named_cols)
  remove_cols <- grep("^-", named_cols, value = TRUE)
  common_cols <- common_cols[!common_cols %in% gsub("^-", "", remove_cols)]
  common_cols <- c(common_cols, named_cols)

  common_rows <- lapply(
    .fcst,
    function(x) {
      dplyr::arrange(
        dplyr::distinct(
          dplyr::select(
            x,
            dplyr::any_of(common_cols),
          )
        ),
        dplyr::across(dplyr::any_of(common_cols))
      )
    }
  )

  common_rows <- Reduce(
    function(x, y) suppressMessages(dplyr::inner_join(x, y)),
    common_rows
  )

  if (rows_only) {
    return(common_rows)
  }

  all_identical <- all(
    purrr::map2_lgl(
      1:(length(common_rows) - 1),
      2:length(common_rows),
      ~identical(common_rows[[.x]], common_rows[[.y]])
    )
  )

  if (all_identical) {
    return(.fcst)
  }

  suppressMessages(
    suppressWarnings(
      join_to_fcst(.fcst, common_rows, force = TRUE)
    )
  )

}


#' Remove harp classes
#'
#' In some cases you may need to remove harp classes from a data frame, for
#' example if methods do not exist. Use this function to remove all harp related
#' classes from the object
#'
#' @param x Any object.
#'
#' @return `x` with all harp classes removed.
#' @export
#'
#' @examples
#' class(det_point_df)
#' class(deharp(det_point_df))
deharp <- function(x) {
  UseMethod("deharp")
}

#' @export
deharp.default <- function(x) {
  class(x) <- grep("harp_", class(x), value = TRUE, invert = TRUE)
  x
}

#' @export
deharp.harp_list <- function(x) {
  (lapply(x, deharp))
}

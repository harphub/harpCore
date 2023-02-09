# dplyr / tidyr methods for harp_list (and other harp classes)

#' Create, modify and delete columns
#'
#' This is the \code{harp_list} method for \code{\link[dplyr]{mutate}()}. It
#' works in exactly the same way except that a \code{harp_list} is returned.
#' To use this function, the \code{dplyr} package must be attached and the
#' \code{.harp_list} suffix can be dropped. Note that
#' \code{\link[dplyr]{transmute}()} does the same as
#' \code{\link[dplyr]{mutate}()}, except that only the modified and / or
#' created columns are kept.
#'
#' @importFrom dplyr mutate
#' @inheritParams dplyr::mutate
#' @export
mutate.harp_list <- function(.data, ...) {
  as_harp_list(lapply(.data, mutate, ...))
}

#' @rdname mutate.harp_list
#' @importFrom dplyr transmute
#' @export
transmute.harp_list <- function(.data, ...) {
  as_harp_list(lapply(.data, transmute, ...))
}

#' Subset columns using their names and types
#'
#' This is the \code{harp_list} method for \code{\link[dplyr]{select}()}. It
#' works in exactly the same way except that a \code{harp_list} is returned.
#' To use this function, the \code{dplyr} package must be attached and the
#' \code{.harp_list} suffix can be dropped.
#'
#' @inheritParams dplyr::select
#' @importFrom dplyr select
#' @export
select.harp_list <- function(.data, ...) {
  as_harp_list(lapply(.data, select, ...))
}

#' Rename columns
#'
#' This is the \code{harp_list} method for \code{\link[dplyr]{rename}()}. It
#' works in exactly the same way except that a \code{harp_list} is returned.
#' To use this function, the \code{dplyr} package must be attached and the
#' \code{.harp_list} suffix can be dropped.
#'
#' For \code{rename_with}, the \code{.fn} argument is mandatory and following
#' \code{\link[dplyr]{rename}}, the \code{.cols} argument selects all columns
#' to rename by default.
#'
#' @inheritParams dplyr::rename
#' @importFrom dplyr rename
#' @export
rename.harp_list <- function(.data, ...) {
  as_harp_list(lapply(.data, rename, ...))
}

#' @rdname rename.harp_list
#' @importFrom dplyr rename_with
#' @export
rename_with.harp_list <- function(.data, .fn, ...) {
  as_harp_list(lapply(.data, rename_with, .fn, ...))
}

#' Subset rows using column values
#'
#' This is the \code{harp_list} method for \code{\link[dplyr]{filter}()}. It
#' works in exactly the same way except that a \code{harp_list} is returned.
#' To use this function, the \code{dplyr} package must be attached and the
#' \code{.harp_list} suffix can be dropped.
#'
#' @importFrom dplyr filter
#' @inheritParams dplyr::filter
#' @export
filter.harp_list <- function(.data, ...) {
  as_harp_list(lapply(.data, dplyr::filter, ...))
}

# dplyr::filter needs to be exported to prevent
# Warning: declared S3 method 'filter.tbl_time' not found
# because of stats::filter
#' @export
#'
dplyr::filter

#' Arrange rows by column values
#'
#' This is the \code{harp_list} method for \code{\link[dplyr]{arrange}()}.
#' It works in exactly the same way except that a \code{harp_list} is returned.
#' To use this function, the \code{dplyr} package must be attached and the
#' \code{.harp_list} suffix can be dropped.
#'
#' @inheritParams dplyr::arrange
#' @importFrom dplyr arrange
#' @export
arrange.harp_list <- function(.data, ..., .by_group = FALSE) {
  as_harp_list(lapply(.data, arrange, ..., .by_group))
}

#' Extract a single column
#'
#' This is the \code{harp_list} method for \code{\link[dplyr]{pull}()}. It
#' works in exactly the same way except that a list of vectors is returned.
#' To use this function, the \code{dplyr} package must be attached and the
#' \code{.harp_list} suffix can be dropped.
#'
#' @inheritParams dplyr::pull
#' @importFrom dplyr pull
#' @export
pull.harp_list <- function(.data, ...) {
  as_harp_list(lapply(.data, pull, ...))
}

#' Bind data frames in a list
#'
#' `bind` is a wrapper around \code{\link[dplyr]{bind_rows}} with a dedicated
#' method for harp_list objects. In all other cases
#' \code{\link[dplyr]{bind_rows}} is called.
#'
#' For harp_list objects, the name of each element in the list is added to each
#' data frame as a column, with the heading as given in the \code{.id}
#' argument. For forecast data the prefix before "_det" for deteriministic
#' forcasts and the prefix before "_mbr***" for ensemble forecasts are
#' removed from the column headings. For multimodel ensembles, this could
#' lead to problems with columns having the same heading.
#'
#' It should be noted that no check is made on the classes of the data frames
#' in the harp_list object, and the class of the first is used in the output.
#'
#' @param ... A list or a harp_list object
#' @param .id The name of the column to be used to identify each element of
#'  the list in the resulting data frame
#'
#' @return A data frame with the class of the first data frame in the list
#' @export
#'
#' @examples
#' bind(
#'   as_harp_list(
#'     a = as_harp_df(data.frame(
#'       validdate = seq_dates(2021010100, 2021010123),
#'       a_det = runif(24)
#'     )),
#'     b = as_harp_df(data.frame(
#'       validdate = seq_dates(2021010100, 2021010123),
#'       b_det = runif(24)
#'     ))
#'   )
#' )
#' bind(
#'   as_harp_list(
#'     a = as_harp_df(data.frame(
#'       validdate = seq_dates(2021010100, 2021010123),
#'       a_mbr000  = runif(24),
#'       a_mbr001  = runif(24)
#'     )),
#'     b = as_harp_df(data.frame(
#'       validdate = seq_dates(2021010100, 2021010123),
#'       b_mbr000 = runif(24),
#'       b_mbr001 = runif(24)
#'     ))
#'   )
#' )

bind <- function(..., .id = NULL) {
  UseMethod("bind")
}

#' @export
bind.default <- function(..., .id = NULL) {
  dplyr::bind_rows(..., .id = .id)
}

#' @export
bind.harp_list <- function(.harp_list, .id = "fcst_model", ...) {
  .harp_list <- dplyr::rename_with(
    .harp_list,
    ~gsub("[[:graph:]]+_(?=mbr[[:digit:]]+|det$)", "", .x, perl = TRUE),
    .cols = dplyr::matches("mbr[[:digit:]]{3}|det$")
  )
  # geolist columns need to be made into simple list columns first,
  # and then reconstructed into geolists.
  add_suffix <- function(x) {
    if (length(x) == 0) return(as.character())
    paste0(x, "_geolist")
  }
  .harp_list <- dplyr::rename_with(
    .harp_list, add_suffix, dplyr::where(is_geolist)
  )

  .harp_list <- dplyr::mutate(
    .harp_list, dplyr::across(dplyr::where(is_geolist), unclass)
  )

  .harp_list <- dplyr::bind_rows(
    mapply(
      function(x, y) {x[[.id]] <- y; x},
      .harp_list,
      names(.harp_list),
      SIMPLIFY = FALSE
    )
  )

  check_geolist_cols(.harp_list)

  .harp_list <- dplyr::mutate(
    .harp_list, dplyr::across(dplyr::matches("_geolist$"), as_geolist)
  )

  .harp_list <- dplyr::rename_with(
    .harp_list, function(x) gsub("_geolist$", "", x)
  )

  dplyr::relocate(
    .harp_list, dplyr::all_of(.id), .before = dplyr::everything()
  )
}

check_geolist_cols <- function(x) {
  geolist_cols <- grep("_geolist$", colnames(x))
  if (length(x) < 1) {
    return()
  }
  bad_cols <- 0
  bad_col_names <- as.character()
  for (i in geolist_cols) {
    if (!check_geolist_domains(x[[i]])) {
      bad_cols <- bad_cols + 1
      bad_col_names[bad_cols] <- sub("_geolist$", "", colnames(x)[i])
    }
  }
  if (bad_cols > 0) {
    stop(
      "Cannot bind data frames.\n Domain mismatch in columns: ",
      paste0("`", paste(bad_col_names, collapse = "`, `"), "`")
    )
  }
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


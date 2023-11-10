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

# harp_df methods

# # Can't get group_by to work in a package. The only solution is for users to
# # run as_harp_df after grouping.
# #' @importFrom dplyr group_by
# #' @export
# #group_by.harp_df <- function(.data, ...) {
# #  .data <- dplyr::group_by(deharp(.data), ...)
# #  if (is.element("valid_dttm", colnames(.data))) {
# #    return(as_harp_df(.data))
# #  }
# #  .data
# #}
# #
# ##' @importFrom dplyr ungroup
# ##' @export
# #ungroup.harp_df <- function(x, ...) {
# #  x <- dplyr::ungroup(deharp(x), ...)
# #  if (is.element("valid_dttm", colnames(x))) {
# #    return(as_harp_df(x))
# #  }
# #  x
# #}
#
# #' #' @importFrom dplyr dplyr_reconstruct
# #' #' @export
# #' dplyr_reconstruct.harp_df <- function(data, ...) {
# #'   as_harp_df(NextMethod())
# #' }
# #'
# #' #' @importFrom dplyr dplyr_col_modify
# #' #' @export
# #' dplyr_col_modify.harp_df <- function(data, cols) {
# #'   as_harp_df(NextMethod())
# #' }
# #'

#' Bind data frames in a list
#'
#' \code{bind} is a wrapper around \code{\link[dplyr]{bind_rows}} with a
#' dedicated method for harp_list objects. In all other cases
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
#'       valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)),
#'       a_det = runif(24)
#'     )),
#'     b = as_harp_df(data.frame(
#'       valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)),
#'       b_det = runif(24)
#'     ))
#'   )
#' )
#' bind(
#'   as_harp_list(
#'     a = as_harp_df(data.frame(
#'       valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)),
#'       a_mbr000  = runif(24),
#'       a_mbr001  = runif(24)
#'     )),
#'     b = as_harp_df(data.frame(
#'       valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)),
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
  .harp_list <- unclass(.harp_list)
  ens_indices <- which(
    sapply(.harp_list, function(x) any(grepl("harp_ens", class(x))))
  )
  if (length(ens_indices) > 0) {
    .harp_list[ens_indices] <- lapply(.harp_list[ens_indices], pivot_members)
  }
  dplyr::bind_rows(.harp_list, .id = .id)
}

### Joins
check_join_inputs <- function(x, y) {
  if (is_harp_list(y)) {
    if (length(y) != length(x)) {
      stop("If joining harp_lists, x and y must be the same length.")
    }
    return(TRUE)
  }
  if (!is.data.frame(y)) {
    stop(
      "For joining to a harp_list, y must be a data frame or a harp_list ",
      "of the same length as x."
    )
  }
  FALSE
}

#' Mutating joins for harp_lists
#'
#' These are the \code{harp_list} methods for join functions, for example
#' \code{\link[dplyr]{inner_join}} from the \code{dplyr} package. In the case
#' of x being a \code{harp_list}, \code{y} can either be a data frame or
#' another \code{harp_list} that is the same length of \code{x}. If
#' \code{y} is a data frame, an attempt will be made to join \code{y} to
#' each data frame in the \code{harp_list}, \code{x}. If \code{y} is
#' a \code{harp_list}, data frames in corresponding elements of \code{x} and
#' \code{y} will be joined. Note that this is done on the basis of location
#' in the list and names are not taken into account. Names in the output are
#' always taken from the names of \code{x}.
#'
#' @return A \code{harp_list} with the same names as \code{x}.
#' @inherit dplyr description
#' @name mutate-joins-harp-list
NULL

#' @inheritParams dplyr::inner_join
#' @inheritDotParams dplyr::inner_join -x -y -copy
#' @importFrom dplyr inner_join
#' @rdname mutate-joins-harp-list
#' @export
inner_join.harp_list <- function(x, y, by = NULL, ...) {
  y_geo <- check_join_inputs(x, y)
  if (y_geo) {
    return(
      as_harp_list(
        mapply(
          inner_join, x, y, MoreArgs = list(by = by, ...), SIMPLIFY = FALSE
        )
      )
    )
  }
  as_harp_list(lapply(x, inner_join, y, by, ...))
}

#' @inheritParams dplyr::left_join
#' @inheritDotParams dplyr::left_join -x -y -copy
#' @importFrom dplyr left_join
#' @rdname mutate-joins-harp-list
#' @export
left_join.harp_list <- function(x, y, by = NULL, ...) {
  y_geo <- check_join_inputs(x, y)
  if (y_geo) {
    return(
      as_harp_list(
        mapply(
          left_join, x, y, MoreArgs = list(by = by, ...), SIMPLIFY = FALSE
        )
      )
    )
  }
  as_harp_list(lapply(x, left_join, y, by, ...))
}

#' @inheritParams dplyr::right_join
#' @inheritDotParams dplyr::right_join -x -y -copy
#' @importFrom dplyr right_join
#' @rdname mutate-joins-harp-list
#' @export
right_join.harp_list <- function(x, y, by = NULL, ...) {
  y_geo <- check_join_inputs(x, y)
  if (y_geo) {
    return(
      as_harp_list(
        mapply(
          right_join, x, y, MoreArgs = list(by = by, ...), SIMPLIFY = FALSE
        )
      )
    )
  }
  as_harp_list(lapply(x, right_join, y, by, ...))
}

#' @inheritParams dplyr::full_join
#' @inheritDotParams dplyr::full_join -x -y -copy
#' @importFrom dplyr full_join
#' @rdname mutate-joins-harp-list
#' @export
full_join.harp_list <- function(x, y, by = NULL, ...) {
  y_geo <- check_join_inputs(x, y)
  if (y_geo) {
    return(
      as_harp_list(
        mapply(
          full_join, x, y, MoreArgs = list(by = by, ...), SIMPLIFY = FALSE
        )
      )
    )
  }
  as_harp_list(lapply(x, full_join, y, by, ...))
}

#' Filtering joins for harp_lists
#'
#' These are the \code{harp_list} methods for filtering join functions, e.g.
#' \code{\link[dplyr]{semi_join}} from the \code{dplyr} package. In the case
#' of x being a \code{harp_list}, \code{y} can either be a data frame or
#' another \code{harp_list} that is the same length of \code{x}. If
#' \code{y} is a data frame, an attempt will be made to join \code{y} to
#' each data frame in the \code{harp_list}, \code{x}. If \code{y} is
#' a \code{harp_list}, data frames in corresponding elements of \code{x} and
#' \code{y} will be joined. Note that this is done on the basis of location
#' in the list and names are not taken into account. Names in the output are
#' always taken from the names of \code{x}.
#'
#' @return A \code{harp_list} with the same names as \code{x}.
#' @inherit dplyr description
#' @name filter-joins-harp-list
NULL

#' @inheritParams dplyr::semi_join
#' @inheritDotParams dplyr::semi_join -x -y -copy
#' @importFrom dplyr semi_join
#' @rdname filter-joins-harp-list
#' @export
semi_join.harp_list <- function(x, y, by = NULL, ...) {
  y_geo <- check_join_inputs(x, y)
  if (y_geo) {
    return(
      as_harp_list(
        mapply(
          semi_join, x, y, MoreArgs = list(by = by, ...), SIMPLIFY = FALSE
        )
      )
    )
  }
  as_harp_list(lapply(x, semi_join, y, by, ...))
}

#' @inheritParams dplyr::anti_join
#' @inheritDotParams dplyr::anti_join -x -y -copy
#' @importFrom dplyr anti_join
#' @rdname filter-joins-harp-list
#' @export
anti_join.harp_list <- function(x, y, by = NULL, ...) {
  y_geo <- check_join_inputs(x, y)
  if (y_geo) {
    return(
      as_harp_list(
        mapply(
          anti_join, x, y, MoreArgs = list(by = by, ...), SIMPLIFY = FALSE
        )
      )
    )
  }
  as_harp_list(lapply(x, anti_join, y, by, ...))
}

# Define `:=` so it can be used (though it doesn't actually do anything so
# an abort works fine. This is copied from rlang)
`:=` <- function(x, y) {
  rlang::abort(
    "`:=` can only be used within dynamic dots.",
    call = rlang::caller_env()
  )
}

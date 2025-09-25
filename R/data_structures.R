# Data structures for harp - allow dispatch to correct methods
#

# The foundational structure is the harp data frame: harp_df
# and from this other class are derived based on the content
# of the data frame

# Helper function to determine depth of a list
depth <- function(x) {
  if (!is.list(x)) {
    return(0)  # Base case: not a list, depth is 0
  } else if (length(x) == 0) {
    return(1)  # Empty list has depth 1
  } else {
    return(1 + max(sapply(x, depth)))  # Recur for all elements and take max depth
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
  if (length(intersect(c("fcst_model", "fcst"), col_names)) == 2) {
    classes <- c(sub("harp", "harp_det", classes[1]), classes)
  }
  if (length(intersect(c("anl_model", "anl"), col_names)) == 2) {
    classes <- c(sub("harp", "harp_anl", classes[1]), classes)
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
print.harp_ens_point_df_long <- function(x, ...) {
  cat(cli::col_green("::ensemble point forecast [[long]]:: "))
  NextMethod()
}

#' @export
print.harp_ens_grid_df_long <- function(x, ...) {
  cat(cli::col_green("::ensemble gridded forecast [[long]]:: "))
  NextMethod()
}

#' @export
print.harp_ens_xs_df_long <- function(x, ...) {
  cat(cli::col_green("::ensemble cross-section forecast [[long]]:: "))
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
#'     valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)),
#'     a_det = runif(24)
#'   )),
#'   b = as_harp_df(data.frame(
#'     valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)),
#'     b_det = runif(24)
#'   ))
#' )
#' as_harp_list(
#'   a = as_harp_df(data.frame(
#'     valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)),
#'     a_mbr000  = runif(24),
#'     a_mbr001  = runif(24)
#'   )),
#'   b = as_harp_df(data.frame(
#'     valid_dttm = as_dttm(seq_dttm(2021010100, 2021010123)),
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

  valid_nrows <- vapply(x, function(d) get_data_num_rows(d) > 0, logical(1))
  x <- x[valid_nrows]
  if (length(x) < 1) {
    cli::cli_abort(c(
      "No data to convert to a {.cls harp_list}.",
      "x" = paste(
        "You supplied a list where all entries are either",
        "not {.cls data.frame} or have 0 rows."
      )
    ))
  }
  valid_types <- vapply(
    x,
    function(d) inherits(d, "harp_df") || inherits(d, "arrow_dplyr_query"),
    logical(1)
  )
  valid_entries <- valid_nrows & valid_types
  if (length(which(valid_entries)) < length(x)) {
    invalid_entries <- which(!valid_entries)
    if (!is.null(names(x))) {
      invalid_entries <- names(x)[invalid_entries]
    }
    cli::cli_abort(c(
      "Not all elements are valid",
      "!" = paste(
        "{invalid_entries} either have 0 rows or are not a {.cls harp_df}",
        "data frame or an uncollected {.cls arrow_dplyr_query}."
      )
    ))
    x <- x[valid_entries]
  }
  if (is.null(names(x))) {
    names_from_df <- unlist(lapply(x, get_model_name))
    if (length(names_from_df) != length(x)) {
      cli::cli_abort(c(
        "Cannot get model names from data.",
        "x" = "You supplied data without names and cannot infer names.",
        "i" = "Add names before calling {.fun as_harp_list}."
      ))
    }
    names(x) <- names_from_df
  }
  if (all(sapply(x, inherits, "harp_df"))) {
    return(structure(x, class = c("harp_list", class(x))))
  }
  if (any(sapply(x, inherits, "arrow_dplyr_query"))) {
    return(structure(x, class = c("harp_list_uncollected", class(x))))
  }
  cli::cli_abort(c(
    "Cannot create a {.cls harp_list}.",
    "x" = paste(
      "You provided a list that comprises",
      "{.cls {unique(sapply(x, class))}} elements."
    ),
    "i" = paste(
      "All elements should be {.cls harp_df} data frames",
      "or uncollected {.cls arrow_dplyr_query} uncollected dataset."
    )
  ))

}

get_model_name <- function(df) {
  model_name <- unique(suppressWarnings(df$sub_model))
  if (length(model_name) != 1) {
    model_name <- unique(suppressWarnings(df$fcst_model))
  }
  if (length(model_name) != 1) {
    model_name <- unique(suppressWarnings(df$anl_model))
  }
  if (length(model_name) != 1) {
    model_name <- NULL
  }
  model_name
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

#' Convert an ensemble forecast to deterministic.
#'
#' @param x A harp data frame or harp list
#' @param member Does not need to be supplied if there is only one ensemble
#'   member in `x`. Otherwise can be the member number, or the full name of
#'   the member column to treat as deterministic.
#' @param sub_model For multimodel ensembles there may be columns with the
#'   same member number. Set to the name of the sub-model to select `member` for
#'   that sub-model.
#'
#' @return A harp deterministic data frame.
#' @export
#'
#' @examples
#' as_det(ens_point_df, 0)
#' as_det(ens_grid_df, 1)
#' as_det(ens_point_list, 0)
#'
#' # Name the member column explicitly
#' as_det(ens_point_df, "point_mbr000")
as_det <- function(x, member = NULL, sub_model = NULL) {
  UseMethod("as_det")
}

#' @export
as_det.harp_ens_point_df <- function(x, member = NULL, sub_model = NULL) {

  member_cols <- grep("_mbr[[:digit:]]{3}", colnames(x), value = TRUE)
  num_members <- length(member_cols)

  if (num_members == 0) {
    cli::cli_abort(c(
      "No ensemble members found in {.arg x}"
    ))
  }

  if (num_members == 1) {
    if (is.null(member)) {
      member <- sub(
        "[[:graph:]]*_(?=mbr[[:digit:]]{3})", "", member_cols, perl = TRUE
      )
    }
  }

  if (is.null(member) && num_members > 1) {
    cli::cli_abort(c(
      "Must specify a member for enesmbles with more than 1 member:",
      "i" = "There {?is/are} are {num_members} member{?s} in {.arg x}.",
      "x" = "{.arg member} is set to NULL."
    ))
  }

  non_member_cols <- grep(
    "_mbr[[:digit:]]{3}", colnames(x), value = TRUE, invert = TRUE
  )

  if (is.numeric(member)) {
    member_regex <- paste0("_mbr", formatC(member, width = 3, flag = "0"))
  }
  if (is.character(member)) {
    member_regex <- member
  }

  len <- length(member)
  if (!exists("member_regex") || len > 1) {
    cli::cli_abort(c(
      "{.arg member} must be length 1 numeric or character vector.",
      "x" = "You supplied a length {len} {.cls {class(member)}} vector."
    ))
  }

  member_cols <- grep(member_regex, colnames(x), value = TRUE)
  num_members <- length(member_cols)

  if (num_members > 1) {
    if (is.null(sub_model)) {
      cli::cli_abort(c(
        "More than 1 member matched in {.arg x}:",
        "x" = "{member_regex} matches columns {member_cols}.",
        "i" = paste(
          "Try setting {.arg sub_model},",
          "or {.arg member} as the full name of the column."
        )
      ))
    }
    member_regex <- paste0(sub_model, member_regex)
    member_cols  <- grep(member_regex, colnames(x), value = TRUE)
    num_members  <- length(member_cols)
    if (num_members > 1) {
      cli::cli_abort(c(
        "More than 1 member matched in {.arg x}:",
        "x" = "{member_regex} matches columns {member_cols}.",
        "i" = paste(
          "Try setting {.arg member} as the full name of the column."
        )
      ))
    }
  }

  if (num_members == 0) {
    cli::cli_abort(c(
      "Must select an existing member.",
      "x" = "\"{member_regex}\" not matched in {.arg x}."
    ))
  }

  fcst_model <- paste0(
    gsub("_mbr[[:digit:]]{3}[[:graph:]]*", "", member_cols), "_det"
  )

  as_harp_df(
    dplyr::select(
      x,
      dplyr::all_of(non_member_cols),
      {{fcst_model}} := dplyr::all_of(member_cols)
    )
  )

}

#' @export
as_det.harp_ens_grid_df <- as_det.harp_ens_point_df

#' @export
as_det.harp_list <- function(x, member = NULL, sub_model = NULL) {
  ens_df <- which(
    vapply(
      x,
      function(v) inherits(v, "harp_ens_point_df") ||
        inherits(v, "harp_ens_grid_df"),
      logical(1)
    )
  )

  if (length(ens_df) < 1) {
    main_classes <- vapply(x, function(v) class(v)[1], character(1))

    cli::cli_abort(c(
      "No ensemble data found in {.arg x}.",
      "i" = "{.arg x} contains entries with main classes {main_classes}"
    ))
  }

  x[ens_df] <- lapply(x[ens_df], as_det, member, sub_model)
  x
}

# Print method for harp_verif
#' @export
print.harp_verif <- function(x, n = NULL, ...) {
  invisible(
    mapply(
      function(x, y) {
        cat(cli::col_green(paste0("::", y, ":: ")))
        print(x, n = n)
        cat("\n")
      },
      x, names(x)
    )
  )
  parameter <- paste(Reduce(union, attr(x, "parameter")), collapse = ", ")
  dttm <- Reduce(union, attr(x, "dttm"))
  dttm_range <- format(
    harpCore::as_dttm(range(dttm)),
    "%R %Z %d %b %Y"
  )
  stations <- Reduce(union, attr(x, "stations"))
  num_stations <- length(stations)
  groupings <- attr(x, "group_vars")
  if (!is.list(groupings)) {
    groupings <- list(groupings)
  }
  if (all(vapply(groupings, is.list, logical(1)))) {
    groupings <- purrr::flatten(groupings)
  }
  groupings <- lapply(
    groupings,
    function(g) g[vapply(g, nchar, integer(1)) > 0]
  )
  groupings <- groupings[vapply(groupings, length, integer(1)) > 0]
  cat(
    cli::col_cyan("--harp verification for "),
    cli::col_magenta(parameter),
    cli::col_cyan("--"),
    sep = ""
  )
  cat(
    "\n",
    cli::col_cyan("# for forecasts from"),
    cli::col_magenta(dttm_range[1]),
    cli::col_cyan("to"),
    cli::col_magenta(dttm_range[2])
  )
  if (num_stations > 0) {
    cat(
      "\n",
      cli::col_cyan("# using"),
      cli::col_magenta(num_stations),
      cli::col_cyan("observation stations")
    )
  }
  if (length(groupings) > 0) {
    cat("\n", cli::col_cyan("# for verification groups: "))
    invisible(
      lapply(
        groupings,
        function(g) {
          g <- glue::glue_collapse(g, sep = ", ", last = " & ")
          cat("\n   ", cli::col_cyan("->"), cli::col_magenta(g))
        }
      )
    )
    cat("\n")
    cli::cli_inform(c(
      "i" = cli::col_silver("use `attributes()` to see detailed metadata")
    ))
  }

}

# Function to get number of rows that also works on an arrow_dplyr_query
get_data_num_rows <- function(x, tries = 1, max_tries = 10) {
  num_rows <- nrow(x)
  if (is.null(num_rows)) {
    return(0)
  }
  if (is.na(num_rows) && tries < max_tries) {
    return(get_data_num_rows(x$.data, tries + 1))
  }
  if (is.null(num_rows)) {
    cli::cli_abort(c(
      "Cannot extract number of rows after nesting down {tries} levels",
      "i" = "There was some prblem accessing any tabular data in this dataset."
    ))
  }
  num_rows
}

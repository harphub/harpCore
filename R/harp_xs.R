# Temporary functions for harp_xs and harp_xs_list - need to have as vctrs
# for harp_xs_list

#' @export
Ops.harp_xs <- function(e1, e2) {
  if (missing(e2)) {
    e2 <- structure(list(), class = "MISSING")
  }
  Ops_xs(.Generic, e1, e2)
}

#' @export
Ops_xs <- function(op, x, y, ...) {
  UseMethod("Ops_xs", x)
}

#' @export
Ops_xs.harp_xs <- function(op, x, y) {
  if (missing(y)) {
    Ops_xs.harp_xs.MISSING(op, x)
  } else {
    UseMethod("Ops_xs.harp_xs", y)
  }
}

#' @export
Ops_xs.numeric <- function(op, x, y) {
  UseMethod("Ops_xs.numeric", y)
}

#' @export
Ops_xs.harp_xs.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

#' @export
Ops_xs.harp_xs.harp_xs <- function(op, x, y) {
  xy <- check_same_xs(x, y) # Also sorts so x and y are in the same order
  check_same_xs_attrs(x, y)
  common_cols <- intersect(colnames(x), colnames(y))
  common_cols <- common_cols[common_cols != "value"]
  op  <- match.fun(op)
  out <- xy$x[common_cols]
  out$value <- op(xy$x$value, xy$y$value)
  new_xs(out)
}

#' @export
Ops_xs.harp_xs.numeric <- function(op, x, y) {
  if (length(y) > 1) {
    stop_incompatible_op(
      op, x, y,
      details = "for <{vec_ptype_full(y)}> of length > 1"
    )
  }
  op <- match.fun(op)
  x$value <- op(x$value, y)
  x
}

#' @export
Ops_xs.harp_xs.NULL <- function(op, x, y) {
  x
}

#' @export
Ops_xs.harp_xs.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

#' @export
Ops_xs.numeric.harp_xs <- function(op, x, y) {
  if (length(x) > 1) {
    stop_incompatible_op(
      op, x, y,
      details = "for <{vec_ptype_full(x)}> of length > 1"
    )
  }
  op <- match.fun(op)
  y$value <- op(x, y$value)
  y
}

#' @export
Ops_xs.harp_xs.MISSING <- function(op, x, y) {
  switch(
    op,
    "-" = minus_xs(x),
    "+" = x,
    rlang::abort(paste(op, "<harp_xs> is not permitted"))
  )
}

minus_xs <- function(xs) {
  xs$value <- -xs$value
  xs
}

new_xs <- function(xs, xs_attrs = NULL) {
  if (missing(xs)) {
    xs <- data.frame(
      distance     = numeric(),
      level        = numeric(),
      level_number = numeric(),
      value        = numeric()
    )
  }
  if (!is.null(xs_attrs)) {
    attributes(xs) <- xs_attrs
  }
  structure(xs, class = c("harp_xs", class(xs)))
}

check_same_xs <- function(x, y) {
  if (nrow(x) != nrow(y)) {
    cli::cli_abort(
      "Cross sections are not the same size."
    )
  }
  level_col_x <- get_level_col(x)
  level_col_y <- get_level_col(y)
  if (level_col_x != level_col_y) {
    cli::cli_abort(c(
      "Incompatible vertical level columns.",
      "x" = "Cross sections have vertical level columns {c(level_col_x, level_col_y)}.",
      "i" = "Cross sections must have the same vertical level columns"
    ))
  }
  x <- dplyr::arrange(x, .data[[level_col_x]], .data[["distance"]])
  y <- dplyr::arrange(y, .data[[level_col_y]], .data[["distance"]])

  if (length(unique(x$distance)) != length(unique(y$distance))) {
    cli::cli_abort(
      "Cross sections are not the same number of horizontal points."
    )
  }

  if (length(unique(x[[level_col_x]])) != length(unique(y[[level_col_y]]))) {
    cli::cli_abort(
      "Cross sections are not the same number of vertical points."
    )
  }

  if (!all(unique(x$distance) == unique(y$distance))) {
    cli::cli_abort(
      "Cross sections do not have the same horizontal distance breaks."
    )
  }

  if (!all(unique(x[[level_col_x]]) == unique(y[[level_col_y]]))) {
    cli::cli_abort(
      "Cross sections do not have the same horizontal distance breaks."
    )
  }
  list(x = x, y = y)
}

check_same_xs_harp_xs_list <- function(xsl) {
  num_itr <- length(xsl) - 1
  out <- lapply(1:num_itr, function(i) {
    if (i == num_itr) {
      check_same_xs(xsl[[i]], xsl[[i + 1]])
    } else {
      check_same_xs(xsl[[i]], xsl[[i + 1]])$x
    }
  })
  out[[num_itr + 1]] <- out[[num_itr]]$y
  out[[num_itr]] <- out[[num_itr]]$x
  new_harp_xs_list(out)
}

check_same_xs_attrs <- function(...) {
  xsl <- list(...)
  if (length(xsl) == 1 && is_harp_xs_list(xsl[[1]])) {
    return(length(unique(lapply(xsl[[1]], attributes))) == 1)
  }
  if (all(vapply(xsl, is_harp_xs, logical(1)))) {
    return(length(unique(lapply(xsl, attributes))) == 1)
  }
  FALSE
}

get_level_col <- function(x) {
  level_cols <- c("level", "pressure", "height")
  level_col  <- intersect(level_cols, colnames(x))
  if (length(level_col) > 1) {
    level_col <- level_col[level_col != "level"]
  }
  if (length(level_col) != 1) {
    cli::cli_abort(c(
      "Cannot find correct vertical level column.",
      "x" = "Found {length(level_col)} possible vertical level column{?s}.",
      "i" = "xs should contain one of {.or {level_cols}}."
    ), call = rlang::caller_env())
  }
  level_col
}


# harp_xs_list - needs redoing as vec_math
new_harp_xs_list <- function(...) {
  xsl <- list(...)
  if (!is_harp_xs(xsl[[1]])) {
    xsl <- xsl[[1]]
  }
  if (!all(vapply(xsl, is_harp_xs, logical(1)))) {
    cli::cli_abort(
      "Cannot create {.cls harp_xs_list}."
    )
  }
  structure(
    xsl, class = union("harp_xs_list", class(xsl))
  )
}

#' @export
mean.harp_xs_list <- function(x, na.rm = FALSE) {
  x <- check_same_xs_harp_xs_list(x)
  check_same_xs_attrs(x)
  out <- x[[1]][colnames(x[[1]]) != "value"]
  out$value <- rowMeans(
    do.call(cbind, lapply(x, function(d) d$value)), na.rm = na.rm
  )
  out
}

#' @export
std_dev.harp_xs_list <- function(x, na.rm = FALSE) {
  x <- check_same_xs_harp_xs_list(x)
  check_same_xs_attrs(x)
  out <- x[[1]][colnames(x[[1]]) != "value"]
  out$value <- matrixStats::rowSds(
    do.call(cbind, lapply(x, function(d) d$value)), na.rm = na.rm
  )
  out
}

#' @export
variance.harp_xs_list <- function(x, na.rm = FALSE) {
  x <- check_same_xs_harp_xs_list(x)
  check_same_xs_attrs(x)
  out <- x[[1]][colnames(x[[1]]) != "value"]
  out$value <- matrixStats::rowVars(
    do.call(cbind, lapply(x, function(d) d$value)), na.rm = na.rm
  )
  out
}

xslapply <- function(x, FUN, ...) {
  new_harp_xs_list(lapply(x, FUN, ...))
}

xslapply2 <- function(x, y, FUN, ...) {
  new_harp_xs_list(mapply(FUN, x, y, MoreArgs = ..., SIMPLIFY = FALSE))
}

#' @export
Ops.harp_xs_list <- function(e1, e2) {
  if (missing(e2)) {
    e2 <- structure(list(), class = "MISSING")
  }
  Ops_xs_list(.Generic, e1, e2)
}

#' @export
Ops_xs_list <- function(op, x, y, ...) {
  UseMethod("Ops_xs_list", x)
}

#' @export
Ops_xs_list.harp_xs_list <- function(op, x, y) {
  if (missing(y)) {
    Ops_xs_lispp$.harp_xs_list.MISSING(op, x)
  } else {
    UseMethod("Ops_xs_list.harp_xs_list", y)
  }
}

#' @export
Ops_xs_list.numeric <- function(op, x, y) {
  UseMethod("Ops_xs_list.numeric", y)
}

#' @export
Ops_xs_list.harp_xs_list.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

#' @export
Ops_xs_list.harp_xs_list.harp_xs_list <- function(op, x, y) {
  x <- check_same_xs_harp_xs_list(x)
  y <- check_same_xs_harp_xs_list(y)
  invisible(check_same_xs_harp_xs_list(c(x, y)))
  check_same_xs_attrs(c(x, y))
  xslapply2(x, y, op)
}

#' @export
Ops_xs_list.harp_xs_list.numeric <- function(op, x, y) {
  if (length(y) > 1) {
    stop_incompatible_op(
      op, x, y,
      details = "for <{vec_ptype_full(y)}> of length > 1"
    )
  }
  xslapply(x, op, y)
}

#' @export
Ops_xs_list.harp_xs_list.NULL <- function(op, x, y) {
  x
}

#' @export
Ops_xs_list.harp_xs_list.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

#' @export
Ops_xs_list.numeric.harp_xs_list <- function(op, x, y) {
  if (length(x) > 1) {
    stop_incompatible_op(
      op, x, y,
      details = "for <{vec_ptype_full(x)}> of length > 1"
    )
  }
  xslapply(y, function(a, b) op(a, b))
}

#' @export
Ops_xs_list.harp_xs_list.MISSING <- function(op, x, y) {
  xslapply(x, op, y)
}

#' @export
print.harp_xs <- function(x, ...) {
  end_points <- attr(x, "end_points")
  a <- which(end_points$end == "a")
  b <- which(end_points$end == "b")
  point_a <- paste0(
    "(", round(end_points$lat[a], 2), "N, ", round(end_points$lon[a], 2), "E)"
  )
  point_b <- paste0(
    "(", round(end_points$lat[b], 2), "N, ", round(end_points$lon[b], 2), "E)"
  )
  level_col  <- get_level_col(x)
  num_levels <- length(unique(x[[level_col]]))
  num_x      <- length(unique(x$distance))
  cat(num_x, "x", num_levels, "Cross section between", point_a, "and", point_b)
  if (!is.null(attr(x, "vertical_coordinate"))) {
    cat("\nVertical coordinate:", attr(x, "vertical_coordinate"))
  }
}

get_xs_dims <- function(xs) {
  level_col <- get_level_col(xs)
  c(length(unique(xs$distance)), length(unique(xs[[level_col]])))
}

#' @importFrom pillar pillar_shaft
#' @export
pillar_shaft.harp_xs_list <- function(x, ...) {
  fmt <- vapply(
    x,
    function(x) paste0(
      "<xs [", paste(get_xs_dims(x), collapse = " x "), "]>"
    ),
    character(1)
  )

  pillar::new_pillar_shaft_simple(pillar::style_subtle(fmt))
}

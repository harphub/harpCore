###########################
# Interpolation to points #
###########################

#' @rdname geo_transform
#' @param points A data frame of geographic points to which to interpolate the
#'   gridded data. The data frame must include the columns "SID" for a unique id
#'   for the point, "lon" for the longitude of the point in decimal degrees and
#'   "lat" for the latitude of the point in decimal degrees. The data frame can
#'   contain other columns, which will be retained in the output.
#' @param force For interpolations that include a mask, it is possible that a
#'   point will be surrounded by 4 masked points. In this case the mask will be
#'   ignored and all 4 points used in the interpolation (the default). Set
#'   `force = TRUE` to force the mask to be applied and set the interpolated to
#'   `NA`.
#'
#' @export
geo_points <- function(
    x,
    points,
    method       = c("bilinear", "nearest", "bicubic"),
    mask         = NULL,
    force        = FALSE,
    weights      = NULL,
    keep_weights = FALSE
) {
  UseMethod("geo_points")
}

#' @export
geo_points.geofield <- function(
    x,
    points,
    method       = c("bilinear", "nearest", "bicubic"),
    mask         = NULL,
    force        = FALSE,
    weights      = NULL,
    keep_weights = FALSE
) {

  x <- latlong_fudge(x)

  if (missing(points) && is.null(weights)) {
    cli::cli_alert_info("Using default points from `station_list`.")
    points <- get("station_list")
  }

  if (is.null(weights) && is.null(points)) {
    stop("Either points or weights must be passed.")
  }

  method <- match.arg(method)

  if (is.null(weights)) {

    ll_cols <- c("lon", "lat")

    if (
      !is.data.frame(points) ||
      all(intersect(ll_cols, colnames(points)) != ll_cols)
    ) {
      rlang::abort(
        "`points` must be a data frame including columns 'lon' and 'lat'."
      )
    }

    options(warn = -1)
    weights <- suppressWarnings(geo_weights_points(
      x,
      points    = points,
      method    = method,
      mask      = mask,
      force     = force
    ))
    options(warn = 0)

  }

  options(warn = -1)
  weights[["point_data"]] = suppressWarnings(
    meteogrid::point.interp(x, method = method, weights = weights)
  )

  options(warn = 0)

  if (!keep_weights) {
    non_weight_cols <- intersect(
      union(
        colnames(get("station_list")),
        c("lon", "lat", "x", "y", "point_data")
      ),
      colnames(weights)
    )
    weights <- weights[non_weight_cols]
  }

  weights <- weights[stats::complete.cases(weights), ]
  attr(weights, "domain") <- get_domain(x)
  weights

}

#' @export
geo_points.harp_geolist <- function(
    x,
    points,
    method       = c("bilinear", "nearest", "bicubic"),
    mask         = NULL,
    force        = FALSE,
    weights      = NULL,
    keep_weights = FALSE
) {

  if (missing(points)) {
    message("Using default points from `station_list`.")
    points <- get("station_list")
  }

  method <- match.arg(method)

  if (is.null(weights)) {
    weights <- geo_weights_points(x[[1]], points, method, mask, force)
  }

  non_weight_cols <- c(
    intersect(colnames(points), colnames(weights)), "point_data"
  )

  points <- dplyr::inner_join(
    points, weights, by = intersect(colnames(points), colnames(weights))
  )

  res <- lapply(
    x, geo_points, points, weights = weights,
    method = method, keep_weights = FALSE
  )

  if (keep_weights) {
    attr(res, "weights") <- weights
  }

  attr(res, "domain") <- get_domain(x)
  res

}

#' @export
geo_points.harp_grid_df <- function(
    x,
    points,
    method       = c("bilinear", "nearest", "bicubic"),
    mask         = NULL,
    force        = FALSE,
    weights      = NULL,
    keep_weights = FALSE
) {

  if (missing(points)) {
    message("Using default points from `station_list`.")
    points <- get("station_list")
  }

  out_cols     <- union(colnames(x), colnames(points))
  method       <- match.arg(method)
  geolist_cols <- colnames(x)[which(sapply(x, is_geolist))]
  domains      <- lapply(x[geolist_cols], get_domain)
  same_domains <- check_same_domain(domains)

  if (same_domains) {
    weights <- geo_weights_points(domains[[1]], points, method, mask, force)
  }

  x <- dplyr::mutate(
    x,
    dplyr::across(
      geolist_cols,
      ~geo_points(.x, points, method, mask, force, weights, keep_weights)
    )
  )

  if (keep_weights) weights_attr <- list()

  for (col in geolist_cols) {
    if (keep_weights) {
      weights_attr[[col]] <- attr(x[[col]], "weights")
    }
    x[[col]] <- lapply(
      x[[col]],
      function(d) {colnames(d)[colnames(d) == "point_data"] <- col; d}
    )
    if (col != geolist_cols[1]) {
      x[[col]] <- lapply(
        x[[col]],
        function(d) d[col]
      )
    }
  }

  x <- as_harp_df(tidyr::unnest(x, geolist_cols))

  if (keep_weights) {
    attr(x, "weights") <- weights_attr
  }

  x <- x[c(setdiff(out_cols, geolist_cols), geolist_cols)]
  attr(x, "domain") <- domains
  x

}

#############################
# Interpolation to new grid #
#############################

#' @rdname geo_transform
#' @param new_grid A `geofield` or `geodomain` on the grid that `x` should be
#'   regridded to. \link{define_domain} can be used to define a new `geodomain`.
#' @param new_mask A `geofield` on the same grid as `new_grid` with grid points
#'   that should not be interpolated to set to 0 or FALSE.
#'
#' @export
geo_regrid <- function(
    x,
    new_grid,
    method       = c("bilinear", "nearest", "bicubic", "upscale"),
    mask         = NULL,
    new_mask     = NULL,
    weights      = NULL,
    keep_weights = FALSE
) {
  UseMethod("geo_regrid")
}

check_args_geo_regrid <- function(
    x, new_grid, mask, new_mask, caller = rlang::caller_env()
) {

  if (missing(new_grid)) return()

  dom <- get_domain(new_grid)

  if (is.null(dom)) {
    rlang::abort(
      "`new_grid` must be a geodomain or a geolfield or geolist.",
      call = caller
    )
  }

  if (!is.null(mask)) {
    mask_error <- "`mask` must be a geofield on the same domain as `x`."

    if (!meteogrid::is.geofield(mask)) {
      rlang::abort(mask_error, call = caller)
    }

    if (!check_same_domain(list(x, new_grid))) {
      rlang::abort(mask_error, call = caller)
    }

    if (is.null(new_mask)) {
      rlang::abort(
        "For masking, both `mask` and `new_mask` should be passed.",
        call = caller
      )
    }

    new_mask_error <- gsub(
      "x", "new_grid", gsub("`mask`", "`new_mask`", mask_error)
    )

    if (!meteogrid::is.geofield(new_mask)) {
      rlang::abort(new_mask_error, call = caller)
    }

    if (!check_same_domain(list(new_grid, new_mask))) {
      rlang::abort(new_mask_error, call = caller)
    }

  } else {

    if (!is.null(new_mask)) {
      rlang::abort(
        "`new_mask` requires `mask` to also be passed.", call = caller
      )
    }

  }

}
#' @export
geo_regrid.geofield <- function(
    x,
    new_grid,
    method       = c("bilinear", "nearest", "bicubic", "upscale"),
    mask         = NULL,
    new_mask     = NULL,
    weights      = NULL,
    keep_weights = FALSE
) {


  check_args_geo_regrid(x, new_grid, mask, new_mask)

  x       <- latlong_fudge(x)
  method  <- match.arg(method)
  dom     <- get_domain(x)

  recompute_weights <- FALSE

  if (!is.null(weights) &&
      !check_same_domain(list(attr(weights, "olddomain"), dom))
  ) {
    arg1 <- "x"
    arg2 <- "weights"
    var1 <- "olddomain"
    cli::cli_warn(c(
      "{.arg {arg1}} and {.var {var1}} in {.arg {arg2}} do not match.",
      "i" = "Recomputing weights"
    ))
    recompute_weights <- TRUE
    weights <- NULL
  }

  if (is.null(weights)) {
    if (missing(new_grid)) {
      arg1 <- "new_grid"
      arg2 <- "weights"
      if (recompute_weights) {
        cli::cli_abort(c(
          "Cannot recompute regridding weights:",
          "i" = "If {.arg {arg2}} is not available, {.arg {arg1}} must be passed.",
          "x" = "{.arg {arg1}} not passed."
        ))
      } else {
        cli::cli_abort(c(
          "Cannot compute regridding weights:",
          "x" = "{.arg {arg1}} not passed."
        ))
      }
    }
    new_dom  <- latlong_fudge(get_domain(new_grid))
    weights  <- geo_weights_regrid(dom, new_dom, method, mask, new_mask)
  }

  if (method == "upscale") {
    res <- meteogrid::upscale_regrid(
      x, attr(weights, "newdomain"), weights = weights
    )
  } else {
    res <- meteogrid::regrid(x, new_dom, weights = weights)
  }

  if (keep_weights) {
    attr(res, "weights") <- weights
  }

  res
}

#' @export
geo_regrid.harp_geolist <- function(
    x,
    new_grid,
    method       = c("bilinear", "nearest", "bicubic", "upscale"),
    mask         = NULL,
    new_mask     = NULL,
    weights      = NULL,
    keep_weights = FALSE
) {

  dom <- get_domain(x)
  check_args_geo_regrid(dom, new_grid, mask, new_mask)

  method <- match.arg(method)

  if (is.null(weights)) {
    weights <- geo_weights_regrid(dom, new_grid, method, mask, new_mask)
  }

  res <- glapply(x, geo_regrid, weights = weights, method = method)
  if (keep_weights) {
    attr(res, "weights") <- weights
  }
  res
}

#' @export
geo_regrid.harp_grid_df <- function(
    x,
    new_grid,
    method       = c("bilinear", "nearest", "bicubic", "upscale"),
    mask         = NULL,
    new_mask     = NULL,
    weights      = NULL,
    keep_weights = FALSE
) {

  method       <- match.arg(method)
  geolist_cols <- colnames(x)[which(sapply(x, is_geolist))]
  domains      <- lapply(x[geolist_cols], get_domain)
  same_domains <- check_same_domain(domains)

  if (same_domains) {
    weights <- geo_weights_regrid(domains[[1]], new_grid, method, mask, new_mask)
  }

  x <- dplyr::mutate(
    x,
    dplyr::across(
      geolist_cols,
      ~geo_regrid(.x, new_grid, method, mask, new_mask, weights, keep_weights)
    )
  )

  if (keep_weights) weights_attr <- lapply(x[geolist_cols], attr, "weights")

  if (keep_weights) {
    attr(x, "weights") <- weights_attr
  }

  x
}

###########################
# Extraction of a subgrid #
###########################

#' @rdname geo_transform
#' @param i1 The x index of the western side of the sub domain.
#' @param i2 The x index of the eastern side of the sub domain.
#' @param j1 The y index of the southern side of the sub domain.
#' @param j2 The y index of the northern side of the sub domain.
#'
#' @export
geo_subgrid <- function(x, i1, i2, j1, j2) {
  UseMethod("geo_subgrid")
}

check_args_geo_subgrid <- function(dom, i1, i2, j1, j2, caller = rlang::caller_env()) {
  args <- c("i1", "i2", "j1", "j2")
  non_numerics <- which(!vapply(list(i1, i2, j1, j2), is.numeric, logical(1)))
  if (any(non_numerics)) {
    args <- args[non_numerics]
    cli::cli_abort(c(
      "Non numeric arguments to function:",
      "x" = "{.arg {args}} {?is/are} not numeric."
    ), call = caller)
  }
  values <- c(i1, i2, j1, j2)
  non_integers <- which((values %% as.integer(values) != 0))
  if (any(non_integers)) {
    args <- args[non_integers]
    values <- as.character(values[non_integers])
    i1 <- floor(i1)
    i2 <- ceiling(i2)
    j1 <- floor(j1)
    j2 <- ceiling(j2)
    new_values <- as.character(c(i1, i2, j1, j2)[non_integers])
    cli::cli_warn(c(
      "Non integer arguments to geo_subgrid:",
      "x" = "{.arg {args}}: ({values}) {?is/are} not {?an integer/integers}.",
      "i" = "Rounding to: {new_values}."
    ))
  }
  if (i1 > i2) {
    args <- c("i1", "i2")
    cli::cli_abort(c(
      "Invalid arguments:",
      "i" = "{.arg {args[1]}} must be less than {.arg {args[2]}}",
      "x" = "{.arg {args[1]}} = {i1}; {.arg {args[2]}} = {i2}"
    ), call = caller)
  }
  if (j1 > j2) {
    args <- c("j1", "j2")
    cli::cli_abort(c(
      "Invalid arguments:",
      "i" = "{.arg {args[1]}} must be less than {.arg {args[2]}}",
      "x" = "{.arg {args[1]}} = {i1}; {.arg {args[2]}} = {i2}"
    ), call = caller)
  }
  if (i1 < 1 || i2 > dom[["nx"]] || j1 < 1 || j2 > dom[["ny"]]) {
    args <- c("i1", "i2", "j1", "j2")
    values <- as.character(c(i1, i2, j1, j2))
    if (i1 < 1) i1 <- 1
    if (i2 > dom[["nx"]]) i2 <- dom[["nx"]]
    if (j1 < 1) j1 <- 1
    if (j2 > dom[["nx"]]) j2 <- dom[["ny"]]
    dims <- as.character(c(1, dom[["nx"]], 1, dom[["ny"]]))
    new_values <- as.character(c(i1, i2, j1, j2))
    cli::cli_warn(c(
      "Subgrid dimensions outside the domain dimensions:",
      "x" = "{.arg {args}}: ({values}) {?is/are} outside domain dimensions ({dims}).",
      "i" = "Adjusting {.arg {args}} to: {new_values}."
    ))
  }
  c(i1, i2, j1, j2)
}

#' @export
geo_subgrid.geofield <- function(x, i1, i2, j1, j2) {
  args <- check_args_geo_subgrid(get_domain(x), i1, i2, j1, j2)
  meteogrid::subgrid(x, args[1], args[2], args[3], args[4])
}

#' @export
geo_subgrid.geodomain <- function(x, i1, i2, j1, j2) {
  args <- check_args_geo_subgrid(x, i1, i2, j1, j2)
  meteogrid::subgrid(x, args[1], args[2], args[3], args[4])
}

#' @export
geo_subgrid.harp_geolist <- function(x, i1, i2, j1, j2) {
  args <- check_args_geo_subgrid(get_domain(x), i1, i2, j1, j2)
  glapply(x, geo_subgrid, args[1], args[2], args[3], args[4])
}

#' @export
geo_subgrid.harp_grid_df <- function(x, i1, i2, j1, j2) {

  geolist_cols <- colnames(x)[which(sapply(x, is_geolist))]
  domains      <- lapply(x[geolist_cols], get_domain)
  same_domains <- check_same_domain(domains)

  if (!same_domains) {
    arg <- "x"
    cli::cli_warn(c(
      "Not all columns in {.arg {arg}} are on the same domain...",
      "i" = "Subgrid output will be on differnt domains."
    ))
  }

  dplyr::mutate(
    x,
    dplyr::across(
      geolist_cols,
      ~geo_subgrid(.x, i1, i2, j1, j2)
    )
  )

}

#####################
# Zoom in to a grid #
#####################

#' @rdname geo_transform
#' @param centre_lon The longitude in decimal degrees of the centre of the
#'   zoomed grid.
#' @param centre_lat The latitude in decimal degrees of the centre of the
#'   zoomed grid.
#' @param length_x The number of grid squares from west to east of the zoomed
#'   grid. If an even number is used, it will be extended by 1 since the zoomed
#'   grid should be centred on the grid square containing
#'   `(centre_lat, centre_lon)`.
#' @param length_y The number of grid squares from south to north of the zoomed
#'   grid. If an even number is used, it will be extended by 1 since the zoomed
#'   grid should be centred on the grid square containing
#'   `(centre_lat, centre_lon)`.
#'
#' @export
geo_zoom <- function(x, centre_lon, centre_lat, length_x, length_y) {
  UseMethod("geo_zoom")
}

get_zoom_indices <-  function(
    x, centre_lon, centre_lat, radius_x, radius_y, caller = rlang::caller_env()
) {

  dom <- get_domain(x)

  cindex <- meteogrid::lalopoint(x, centre_lon, centre_lat)[["index"]][1, ]
  if (any(is.na(cindex))) {
    args   <- c("centre_lon", "centre_lat")[is.na(cindex)]
    values <- c(centre_lon, centre_lat)[is.na(cindex)]
    cli::cli_abort(c(
      "{.arg {args}} {?is/are} outside of domain:",
      "x" = "{.arg {args}} = ({values})"
    ), call = caller)
  }

  list(
    i1 = cindex[["i"]] - radius_x,
    i2 = cindex[["i"]] + radius_x,
    j1 = cindex[["j"]] - radius_y,
    j2 = cindex[["j"]] + radius_y
  )

}

#' @export
geo_zoom.geofield <- function(x, centre_lon, centre_lat, length_x, length_y) {
  length_x <- as.integer(length_x)
  length_y <- as.integer(length_y)
  if (length_x %% 2 != 0) {
    length_x <- length_x + 1
  }
  if (length_y %% 2 != 0) {
    length_y <- length_y + 1
  }
  indices <- get_zoom_indices(
    get_domain(x), centre_lon, centre_lat, length_x / 2, length_y / 2
  )
  do.call(geo_subgrid, c(list(x = x), indices))
}

#' @export
geo_zoom.geodomain <- geo_zoom.geofield

#' @export
geo_zoom.harp_geolist <- geo_zoom.geofield

#' @export
geo_zoom.harp_grid_df <- function(
    x, centre_lon, centre_lat, length_x, length_y
) {

  geolist_cols <- colnames(x)[which(sapply(x, is_geolist))]
  zoomed <- lapply(
    x[geolist_cols],
    function(d) {
      try(
        geo_zoom(d, centre_lon, centre_lat, length_x, length_y),
        silent = TRUE
      )
    }
  )
  error_cols <- which(vapply(zoomed, inherits, logical(1), "try-error"))
  if (length(error_cols) > 0) {
    error_col_names <- geolist_cols[error_cols]
    invisible(mapply(
      function(col, err) {cli::cli_alert(col); cat(err, "\n")},
      error_col_names,
      zoomed[error_cols]
    ))
    cli::cli_abort(c(
      "Errors reported for some columns in data frame:",
      "x" = "{.var {error_col_names}}."
    ))
  }

  x[geolist_cols] <- zoomed
  x

}

#####################################################
# Cross section of a grid (one vertical level only) #
#####################################################

#' @rdname geo_transform
#' @param p1 The geographic location in decimal degrees of the start of the
#'   section. Should be a vector of length 2 with the first value being the
#'   longitude and the second value the latitude.
#' @param p2 The geographic location in decimal degrees of the end of the
#'   section. Should be a vector of length 2 with the first value being the
#'   longitude and the second value the latitude.
#' @param n The number of equally spaced points along the section. The default
#'   is 100.
#' @export
geo_xsection <- function(
    x,
    p1,
    p2,
    n            = 100,
    method       = c("bilinear", "nearest", "bicubic"),
    weights      = NULL,
    keep_weights = FALSE
) {
  UseMethod("geo_xsection")
}

#' @export
geo_xsection.geofield <- function(
    x,
    p1,
    p2,
    n            = 100,
    method       = c("bilinear", "nearest", "bicubic"),
    weights      = NULL,
    keep_weights = FALSE
) {

  x <- latlong_fudge(x)

  if (is.null(weights)) {
    invisible(lapply(list(p1, p2, n), check_numeric))

    p1 <- correct_length(p1, "p1", 2)
    p2 <- correct_length(p2, "p2", 2)
    n  <- correct_length(round_to_integer(n, "n"), "n", 1)

    dom     <- get_domain(x)
    weights <- geo_weights_xsection(dom, p1, p2, n, method)
  }

  res <- geo_points(
    x, weights = weights, method = method, keep_weights = keep_weights
  )

  colnames(res)[colnames(res) == "point_data"] <- "xsection_data"

  res

}

#' @export
geo_xsection.harp_geolist <- function(
    x,
    p1,
    p2,
    n            = 100,
    method       = c("bilinear", "nearest", "bicubic"),
    weights      = NULL,
    keep_weights = FALSE
) {

  if (is.null(weights)) {
    invisible(lapply(list(p1, p2, n), check_numeric))

    a <- correct_length(p1, "a", 2)
    b <- correct_length(p2, "b", 2)
    n <- correct_length(round_to_integer(n, "n"), "n", 1)

    dom     <- get_domain(x)
    weights <- geo_weights_xsection(dom, p1, p2, n, method)
  }

  res <- lapply(
    x, geo_xsection, method = method, weights = weights, keep_weights = FALSE
  )

  if (keep_weights) {
    attr(res, "weights") <- weights
  }

  structure(res, class = c("xslist", "list"), domain = get_domain(x))

}

#' @export
geo_xsection.harp_grid_df <- function(
    x,
    p1,
    p2,
    n            = 100,
    method       = c("bilinear", "nearest", "bicubic"),
    weights      = NULL,
    keep_weights = FALSE
) {

  geolist_cols <- colnames(x)[which(sapply(x, is_geolist))]

  domains <- lapply(x[geolist_cols], get_domain)

  weights <- lapply(
    x[geolist_cols],
    function(d) {
      try(
        geo_weights_xsection(get_domain(d), p1, p2, n, method),
        silent = TRUE
      )
    }
  )
  error_cols <- which(vapply(weights, inherits, logical(1), "try-error"))
  if (length(error_cols) > 0) {
    error_col_names <- geolist_cols[error_cols]
    invisible(mapply(
      function(col, err) {cli::cli_alert(col); cat(err, "\n")},
      error_col_names,
      weights[error_cols]
    ))
    cli::cli_abort(c(
      "Errors reported for some columns in data frame:",
      "x" = "{.var {error_col_names}}."
    ))
  }
  x[geolist_cols] <- mapply(
    function(.x, .y) geo_xsection(.x, weights = .y),
    x[geolist_cols], weights, SIMPLIFY = FALSE
  )
  x <- as_harp_df(x)
  attr(x, "domain") <- domains
  x
}

#######################################
# Upscale a grid by an integer factor #
#######################################

#' @rdname geo_transform
#' @param factor An integer by which to upscale the data. Can be of length 2 to
#'   achieve different upscaling in the x and directions.
#' @param downsample_location When "downsample" is the chosen method, each
#'   pixel in the upscaled field is sampled from a pixel from the original field
#'   that is inside the upscaled pixel. The location of that pixel can be one of
#'   "bottom_left", "bottom_centre", "bottom_right", "left_centre", "centre",
#'   "right_centre", "top_right", "top_centre", "top_left" or "random".
#' @param ... Extra options for `method`.
#'
#' @export
geo_upscale <- function(
  x,
  factor,
  method              = "mean",
  downsample_location = "bottom_left",
  ...
) {
  UseMethod("geo_upscale")
}

#' @export
geo_upscale.geofield <- function(
  x,
  factor,
  method              = "mean",
  downsample_location = "bottom_left",
  ...
) {

  x <- latlong_fudge(x)

  check_numeric(factor, "factor")
  factor <- round_to_integer(factor, "factor")
  if (length(factor) == 1) {
    factor <- rep(factor, 2)
  }

  factor <- correct_length(factor, "factor", 2)

  old_domain    <- meteogrid::as.geodomain(x)
  dom_ext       <- meteogrid::DomainExtent(old_domain)
  dom_crnrs     <- meteogrid::DomainCorners(old_domain)
  new_domain    <- old_domain

  new_domain[["dx"]] <- dom_ext[["dx"]] * factor[1]
  new_domain[["dy"]] <- dom_ext[["dy"]] * factor[2]
  new_domain[["nx"]] <- floor(dom_ext[["nx"]] / factor[1])
  new_domain[["ny"]] <- floor(dom_ext[["ny"]] / factor[2])

  sw0 <- meteogrid::project(
    dom_crnrs[["SW"]], proj = old_domain[["projection"]], inv = FALSE
  )

  sw1 <- c(sw0[["x"]], sw0[["y"]]) +
    c(dom_ext[["dx"]], dom_ext[["dy"]]) * (factor - 1) / 2

  new_domain[["SW"]] <- as.numeric(
    meteogrid::project(sw1, proj = new_domain[["projection"]], inv = TRUE)
  )

  ne1 <- sw1 + c(
    (new_domain[["nx"]] - 1) * new_domain[["dx"]],
    (new_domain[["ny"]] - 1) * new_domain[["dy"]]
  )

  new_domain[["NE"]] <- as.numeric(
    meteogrid::project(ne1, proj = new_domain[["projection"]], inv = TRUE)
  )

  new_domain[["clonlat"]] <- as.numeric(meteogrid::project(
    (ne1 + sw1) / 2, proj = new_domain[["projection"]], inv = TRUE
  ))

  zz <- array(
    x[1:(factor[1] * new_domain[["nx"]]), 1:(factor[2] * new_domain[["ny"]])],
    c(factor[1], new_domain[["nx"]], factor[2], new_domain[["ny"]])
  )

  if (method == "downsample") {
    return(harpCore::geofield(
      downsample(
        x, factor, new_domain[["nx"]], new_domain[["ny"]], downsample_location
      ),
      domain = new_domain
    ))
  }

  if (method == "mean") {
    result <- apply(zz, c(2, 4), sum, ...) / (factor[1] * factor[2])
  } else {
    result <- apply(zz, c(2, 4), match.fun(method), ...)
  }

  harpCore::geofield(result, domain = new_domain)
}

#' @export
geo_upscale.geolist <- function(
  x,
  factor,
  method              = "mean",
  downsample_location = "bottom_left",
  ...
) {
  glapply(x, geo_upscale, factor, method, downsample_location)
}

#' @export
geo_upscale.harp_grid_df <- function(
  x,
  factor,
  method              = "mean",
  downsample_location = "bottom_left",
  ...
) {
  dplyr::mutate(
    x,
    dplyr::across(
      dplyr::where(is_geolist),
      ~geo_upscale(.x, factor, method, downsample_location, ...)
    )
  )
}

#' @export
geo_upscale.harp_list <- function(
  x,
  factor,
  method              = "mean",
  downsample_location = "bottom_left",
  ...
) {
  as_harp_list(
    lapply(x, geo_upscale, factor, method, downsample_location, ...)
  )
}


# Upscale methods
downsample <- function(x, res, nx, ny, location = "bottom_left", ...) {

  x_max    <- res[1]
  y_max    <- res[2]
  x_centre <- ceiling(x_max / 2)
  y_centre <- ceiling(y_max / 2)
  start_ind <- switch(
    location,
    "random"        = ,
    "bottom_left"   = list(x = 1, y = 1),
    "bottom_centre" = list(x = x_centre, y = 1),
    "bottom_right"  = list(x = x_max, y = 1),
    "left_centre"   = list(x = 1, y = y_centre),
    "centre"        = list(x = x_centre, y = y_centre),
    "right_centre"  = list(x = x_max, y = y_centre),
    "top_right"     = list(x = x_max, y = y_max),
    "top_centre"    = list(x = x_centre, y = y_max),
    "top_left"      = list(x = 1, y = y_max),
    list(NA, NA)
  )

  if (any(sapply(start_ind, is.na))) {
    locs <- c(
      "bottom_left",
      "bottom_centre",
      "bottom_right",
      "left_centre",
      "centre",
      "right_centre",
      "top_right",
      "top_centre",
      "top_left",
      "random"
    )
    cli::cli_abort(c(
      "unknown downsample_location: {location}",
      "i" = "{.arg downsample_location} must be one of:",
      "{locs}"
    ))
  }

  x_ind <- seq(start_ind[["x"]], length.out = nx, by = res[1])
  y_ind <- seq(start_ind[["y"]], length.out = ny, by = res[2])

  if (location != "random") {
    return(x[x_ind, y_ind])
  }

  indices <- expand.grid(i = x_ind, j = y_ind)
  indices[["i"]] <- indices[["i"]] +
    sample(seq_len(res[1]) - 1, size = nrow(indices), replace = TRUE)
  indices[["j"]] <- indices[["j"]] +
    sample(seq_len(res[1]) - 1, size = nrow(indices), replace = TRUE)
  indices[["k"]] <- vector_indices(x, as.matrix(indices))
  array(x[indices[["k"]]], c(nx, ny))
}

vector_indices <- function(x, i) {
  replace(x, seq_along(x), seq_along(x))[i]
}

########################
# Generalized function #
########################

#' Geographic transformation of gridded data
#'
#' Gridded data can be transformed from one grid definition to another, to
#' geographic points, to cross sections, to sub-domains of the original grid, or
#' zoomed into the original a grid. The `geo_<transformation>` functions are
#' used to achieve this, while the generalized function `geo_transform` is
#' designed to be used in functions that will take the transformation as an
#' argument.
#'
#' * `geo_points` is used to interpolate from a regular grid to geographic
#' points within the domain of the grid
#' * `geo_regrid` is used to interpolate from one regular grid to another
#' regular grid. This can include reprojection from one grid projection to
#' another.
#' * `geo_xsection` extracts an equally spaced straight line of points between
#' to geographic locations and cen be used to construct a vertical cross section
#' of a 3-dimensional field. For grids that are equally space in longitude -
#' latitude coordinates (e.g. latlong projections) the section can be along a
#' great circle and thus the shortest distance between the two points.
#' * `geo_subgrid` extracts a sub domain of the data without changing the
#' coordinate reference system.
#' * `geo_zoom` is a special case of `geo_subgrid` whereby a sub domain of the
#' data is extracted centred around a geographic point.
#' * `geo_upscale` upscales data from a higher resolution grid to a coarser
#' resolution grid using an integer upscaling factor. The default method is to
#' take the mean of all high resolution pixels inside each coarse resolution
#' pixels, though sampling using the "downsample" method is faster and likely
#' sufficient for upscaling for raster raster plotting.
#' * `geo_transform` is a generalized function that can be used in functions
#' that take the type of geographic transformation as an argument.
#'
#' For transformations that require the interpolation of data (points, regrid
#' and xsection), the method of interpolation can be chosen. The available
#' interpolation methods are nearest neighbour, bilinear and bicubic. In
#' addition, masks can be used to prevent grid points being used in the
#' interpolation - for example if you have a land-sea mask, grid points with a
#' value of 0 or FALSE will not be used in the interpolation.
#'
#' @param x A geofield, geolist, or a data frame with class `harp_grid_df`. For
#'   transformations that do not involve the interpolation of gridded data (e.g.
#'   zoom, subgrid) `x` can also be a geodomain.
#' @param trans The transformation to apply. Can be "points", "regrid",
#'   "xsection", "subgrid", or "zoom".
#' @param opts A list of options for the chosen transformation. The appropriate
#'   \link{geo_opts} function should be used to generate this list.
#' @param method The interpolation method. Can be "nearest" for nearest
#'   neighbour, "bilinear", or "bicubic." The default is "bilinear". For
#'   `geo_upscale`, can be any function that summarises a vector to a
#'   single value and can found with \code{\link[base]{match.fun}}, the default
#'   being "mean". A further option is "downsample", which is described in the
#'   argument for `downsample_location`. For `geo_regrid`, may also be "upscale"
#'   for regridding to a coarser grid. This will take the mean of all the
#'   pixel centroids of `x` that fall within a grid sqaure of the new grid.
#' @param mask A mask to prevent grid points being used in the interpolation.
#'   Should be on the same grid as `x` and grid points with values of 0 or FALSE
#'   will be masked from the interpolation.
#' @param weights Pre-computed weights for the interpolation. Should be the
#'   output from the appropriate \link{geo_weights} function.
#' @param keep_weights Whether to keep weights in the output. If set to TRUE,
#'   the return object will have a "weights" attribute.
#'
#' @return In the case of transformations to points and cross sections, a data
#'   frame. In all other cases an object of the same class as `x` with the
#'   transformation applied.
#' @export
geo_transform <- function(
    x,
    trans = c("points", "regrid", "subgrid", "zoom", "xsection"),
    opts
) {
  UseMethod("geo_transform")
}

geo_transform_all <- function(
    x,
    trans = c("points", "regrid", "subgrid", "zoom", "xsection"),
    opts
) {
  trans <- match.arg(trans)
  trans_func <- get(paste0("geo_", trans))
  if (missing(opts)) {
    opts_fun <- get(paste0("geo_opts_", trans))
    opts <- opts_fun()
  }
  do.call(trans_func, c(list(x = x), opts))
}

#' @export
geo_transform.geofield <- geo_transform_all

#' @export
geo_transform.geodomain <- geo_transform_all

#' @export
geo_transform.harp_geolist <- geo_transform_all

#' @export
geo_transform.harp_grid_df <- geo_transform_all


########################
# Reproject point data #
########################

## Helper function to get the projection
get_projection <- function(x, caller = rlang::caller_env()) {

  if (is_geolist(x) || meteogrid::is.geofield(x)) {
    return(get_domain(x)[["projection"]])
  }

  if (meteogrid::is.geodomain(x)) {
    return(x[["projection"]])
  }

  if (is.list(x) && !is.null(names(x)) && names(x)[1] == "proj") {
    return(x)
  }

  if (is.character(x) && length(x) == 1 && substr(x, 1, 6) == "+proj=") {
    return(meteogrid::proj4.str2list(x))
  }

  classes <- cli::cli_inform("")
  types   <- "a proj string or a meteogrid proj list"
  cli::cli_abort(c(
    "Unknown type for {.arg proj}",
    "i" = "{.arg proj} must be a {.cls geofield}, {.cls geodomain}, {.cls geolist},",
    "i" = "{types}."
    ),
    call = caller
  )
}

## Helper function to fix column names
fix_col_names <- function(col_names, x_col, y_col) {
  new_names <- gsub("projected_", "", col_names)
  if (
    length(which(new_names == x_col)) > 1 ||
    length(which(new_names == y_col)) > 1
  ) {
    return(col_names)
  }
  new_names
}

#' Reproject from or to lat-lon coordinates
#'
#'
#'
#' @param x A data frame
#' @param proj The projection. Can be a `geodomain`, a `geofield`, a `geolist`,
#'   a projection string or a meteogrid projection list. When `inverse = FALSE`,
#'   this is the projection to which locations in lat-lon coordinates are
#'   reprojected, and when `inverse = TRUE`, this the projection from which
#'   locations in projected coordinates are reprojected to lat-lon coordinates.
#' @param x_col,y_col The names of the columns containing the x and y
#'   coordinates to be reprojected. For `inverse = FALSE`, these should be
#'   longitude and latitude in decimal degrees. For `inverse = TRUE`, these
#'   should be eastings and northings in metres.
#' @param crop When `proj` is a `geodomain`, `geofield` or `geolist`, set to
#'   `TRUE` to crop the reprojected locations to only those locations within the
#'   domain.
#' @param inverse Set to `TRUE` to reprojected from projected coordinates to
#'   lat-lon coordinates. The default is `FALSE`
#'
#' @return The input data frame with new columns for the reprojected
#'   coordinates. The projection is added as an attribute. If the data are
#'   cropped, the domain is also added.
#' @export
#'
#' @examples
#' geo_reproject(station_list, det_grid_df$fcst)
#'
#' # Crop to domain
#' geo_reproject(station_list, det_grid_df$fcst, crop = TRUE)
#'
#' # inverse projection
#' projected <- geo_reproject(station_list, det_grid_df$fcst)
#' geo_reproject(
#'   projected, det_grid_df$fcst, x_col = x, y_col = y, inverse = TRUE
#' )
#'
geo_reproject <- function(
  x, proj, x_col = "lon", y_col = "lat", crop = FALSE, inverse = FALSE
) {
  UseMethod("geo_reproject")
}

#' @export
geo_reproject.data.frame <- function(
    x, proj, x_col = "lon", y_col = "lat", crop = FALSE, inverse = FALSE
) {

  x_col <- rlang::ensym(x_col)
  y_col <- rlang::ensym(y_col)

  proj_list <- get_projection(proj)

  x <- dplyr::mutate(
    x,
    projected = meteogrid::project(!!x_col, !!y_col, proj_list, inverse)
  )

  x_col_out <- "projected_x"
  y_col_out <- "projected_y"
  if (inverse) {
    x[["projected"]] <- dplyr::rename(
      x[["projected"]],
      lon = dplyr::all_of("x"),
      lat = dplyr::all_of("y")
    )
    x_col_out <- "projected_lat"
    y_col_out <- "projected_lon"
  }

  x <- tidyr::unnest(x, dplyr::all_of("projected"), names_sep = "_")

  if (!inverse) {
    attr(x, "projection") <- meteogrid::proj4.list2str(proj_list)
  }

  if (!crop) {
    colnames(x) <- fix_col_names(colnames(x), x_col, y_col)
    return(x)
  }

  if (
    !is_geolist(proj) &&
    !meteogrid::is.geodomain(proj) &&
    !meteogrid::is.geofield(proj)
  ) {
    cli::cli_warn(c(
      "For {.arg crop = TRUE}, proj must be a",
      "{.cls geofield}, {.cls geodomain}, or {.cls geolist}"
    ))

    colnames(x) <- fix_col_names(colnames(x), x_col, y_col)
    return(x)
  }

  dom_ext <- meteogrid::DomainExtent(get_domain(proj))

  x <- dplyr::filter(
    x,
    dplyr::between(.data[[x_col_out]], dom_ext[["x0"]], dom_ext[["x1"]]),
    dplyr::between(.data[[y_col_out]], dom_ext[["y0"]], dom_ext[["y1"]])
  )

  colnames(x) <- fix_col_names(colnames(x), x_col, y_col)
  attr(x, "domain") <- get_domain(proj)
  x
}

#' @export
geo_reproject.harp_df <- function(
    x, proj, x_col = "lon", y_col = "lat", crop = FALSE, inverse = FALSE
) {
  as_harp_df(NextMethod())
}

#################
# Make a domain #
#################

check_numeric <- function(x, arg, caller = rlang::caller_env()) {
  if (!is.numeric(x)) {
    value <- as.character(x)
    cli::cli_abort(c(
      "{.arg {arg}} must be numeric.",
      "x" = "You've supplied a {.cls {class(x)}} vector."
    ), call = caller)
  }
}

round_to_integer <- function(x, arg, caller = rlang::caller_env()) {
  if (any(round(x) %% x != 0)) {
    value <- as.character(x)
    new_value <- as.character(round(x))
    cli::cli_warn(c(
      "{.arg {arg}} should be an integer:",
      "i" = "Rounding {.arg {arg}} = {value} to {new_value}"
    ))
    x <- round(x)
  }
  x
}

correct_length <- function(x, arg, len, caller = rlang::caller_env()) {
  if (length(x) < len) {
    cli::cli_abort(c(
      "{.arg {arg}} should be a vector with {len} {?element/elements}",
      "x" = "{.arg {arg}} has {length(x)} {?element/elements}."
    ), call = caller)
  }
  if (length(x) > len) {
    cli::cli_warn(c(
      "{.arg {arg}} should be a vector with {len} {?element/elements}",
      "i" = "Truncating {.ar {arg}} to first {len} {?element/elements}"
    ))
    x <- x[1:len]
  }
  x
}

tranverse_mercator <- function(ref_lon, ref_lat, tilt) {
  if (abs(ref_lat) < 0.01 && abs(tilt) < 0.01) {
    return(list(proj = "merc", lon_0 = ref_lon))
  }
  if (abs(abs(tilt) - 90) < 1e-05) {
    return(list(proj = "tmerc", lon_0 = ref_lon, lat_0 = ref_lat))
  }
  if (abs(tilt) < 1e-05) {
    return(list(proj = "somerc", lon_0 = ref_lon, lat_0 = ref_lat))
  }
  if (tilt > 0) {
    return(list(
      proj = "omerc", lonc = ref_lon, lat_0 = ref_lat,
      alpha = -90 + tilt, no_rot = NA
    ))
  }
  list(
    proj = "omerc", lonc = ref_lon, lat_0 = ref_lat,
    alpha = 90 + tilt, no_rot = NA
  )
}

#' Define a geodomain
#'
#' This function is used to define a new domain with a regular grid. At a
#' minimum, the projection, geographic location of the centre of the domain and
#' number and horizontal resolution of the grid points must be provided.
#'
#' @param proj The projection of the domain. This can be the name of the
#'   projection or a projection string.
#' @param centre_lon,centre_lat The longitude and latitude of the centre of the
#'   domain in decimal degrees.
#' @param nxny The number of grid points in the x and y directions. Should be a
#'   vector of length 2 with the number of grid points in the x direction first.
#'   If only 1 value is given it is assumed to be the same in both directions.
#' @param dxdy The horizontal resolution of the grid in the x and y directions.
#'   For lat-lon projections this should be in decimal degrees, otherwise should
#'   be in metres. Should be a vector of length 2 with the resolution in the x
#'   direction first.
#' @param ref_lon,ref_lat The reference longitude and latitude of the projection
#'   if relevant to the projection. Ignored if `proj` is a projection string.
#' @param exey If defining a grid with an extension zone, a vector length 2 with
#'   the number of grid points in the x and y directions of the extension zone.
#'   If only 1 value is given it is assumed to be the same in both directions.
#' @param tilt The tilt used in a rotated Mercator projection.
#' @param R The radius of the earth in metres. The default is 6371229m.
#' @param ... Other arguments describing the shape of the earth in `proj`
#'   format.
#'
#' @return A `geodomain`
#' @export
#'
#' @examples
#' dd <- define_domain(10, 60, 1000, 2500) # Default lambert projection
#' plot(dd)
#'
#' dd <- define_domain(0, 0, c(360, 180), 1, "latlong") # Whole earth
#' plot(dd)
#'
#' # Pass the projection as a proj string
#' dd <- define_domain(
#'   10, 60, 1000, 2500,
#'   proj = "+proj=lcc +lon_0=15 +lat_0=63.3 +lat_1=63.3 +lat_2=63.3 +R=6371000"
#' )
#' plot(dd)
define_domain <- function(
    centre_lon,
    centre_lat,
    nxny,
    dxdy,
    proj = c(
      "lambert", "lcc", "merc", "mercator", "omerc", "tmerc", "somerc", "lalo",
      "longlat", "latlong", "ob_tran", "rot_longlat", "rot_latlong", "RotLatLon",
      "stere", "stereo", "stereographic"
    ),
    ref_lon,
    ref_lat,
    exey = NULL,
    tilt = 0,
    R    = 6371229,
    ...
) {

  is_proj_str <- FALSE
  projTry <- try(match.arg(proj), silent = TRUE)
  if (inherits(projTry, "try-error")) {
    if (length(proj) == 1 && substring(proj, 1, 6) == "+proj=") {
      is_proj_str <- TRUE
    }
  }
  if (!is_proj_str) {
    proj <- match.arg(proj)
    proj <- switch(
      proj,
      "lambert"       = "lcc",
      "mercator"      = "merc",
      "lalo"          = ,
      "longlat"       = "latlong",
      "omerc"         = ,
      "somerc"        = "tmerc",
      "rot_longlat"   = ,
      "rot_latlong"   = ,
      "RotLatLon"     = "ob_tran",
      "stereo"        = ,
      "stereogrpahic" = "stere",
      proj
    )
  }

  if (length(nxny) == 1) nxny <- rep(nxny, 2)
  if (length(dxdy) == 1) dxdy <- rep(dxdy, 2)

  check_numeric(nxny)
  check_numeric(dxdy)

  check_numeric(centre_lon)
  check_numeric(centre_lat)

  if (missing(ref_lon)) ref_lon <- centre_lon
  if (missing(ref_lat)) ref_lat <- centre_lat

  nxny <- correct_length(round_to_integer(nxny, "nxny"), "nxny", 2)
  dxdy <- correct_length(dxdy, "dxdy", 2)

  if (!is.null(exey)) {
    if (length(exey) == 1) exey <- rep(exey, 2)
    check_numeric(exey)
    exey <- correct_length(round.POSIXt(exey))
  }

  if (is_proj_str) {
    projection <- meteogrid::proj4.str2list(proj)
  } else {
    projection <- switch(
      proj,
      "lcc" = list(
        proj = proj, lon_0 = ref_lon,
        lat_0 = ref_lat, lat_1 = ref_lat, lat_2 = ref_lat
      ),
      "merc" = list(
        proj = proj, lon_0 = ref_lon
      ),
      "tmerc" = tranverse_mercator(ref_lon, ref_lat, tilt),
      "latlong" = list(proj = proj),
      "ob_tran" = list(
        proj = proj, o_proj = "latlong",
        o_lat_p = -ref_lat, o_lon_p = 0, lon_0 = ref_lon
      ),
      "stere" = list(proj = proj, lon_0 = ref_lon, lat_0 = ref_lat)
    )

    projection <- c(projection, list(R = R, ...))
  }

  res <- list(
    projection = projection, nx = nxny[1], ny = nxny[2],
    dx = dxdy[1], dy = dxdy[2], clonlat = c(centre_lon, centre_lat)
  )

  if (!is.null(exey)) {
    res[["ex"]] <- exey[1]
    res[["ey"]] <- exey[2]
  }

  res     <- structure(res, class = "geodomain")

  corners <- meteogrid::DomainCorners(res)
  #res[["clonlat"]] <- NULL
  res[["SW"]] <- unlist(corners[["SW"]], use.names = FALSE)
  res[["NE"]] <- unlist(corners[["NE"]], use.names = FALSE)

  res
}

###############################
# Options for transformations #
###############################

#' Options for different transformations
#'
#' When using \link{geo_transform} the transformation options must be passed as
#' named list appropriate to the transformation. These functions are used to
#' generate such named lists.
#'
#' @name geo_opts

#' @rdname geo_opts
#' @inheritParams geo_transform
#' @export
geo_opts_points <- function(
    points,
    method       = c("bilinear", "nearest", "bicubic"),
    mask         = NULL,
    force        = FALSE,
    weights      = NULL,
    keep_weights = FALSE
) {

  if (missing(points) && is.null(weights)) {
    cli::cli_alert_info("Using default points from `station_list`.")
    points <- get("station_list")
  }

  method <- match.arg(method)

  list(
    points       = points,
    method       = method,
    mask         = mask,
    force        = force,
    weights      = weights,
    keep_weights = keep_weights
  )

}

#' @rdname geo_opts
#' @inheritParams geo_transform
#' @export
geo_opts_regrid <- function(
    new_grid,
    method       = c("bilinear", "nearest", "bicubic"),
    mask         = NULL,
    new_mask     = NULL,
    weights      = NULL,
    keep_weights = FALSE
) {

  method <- match.arg(method)

  list(
    new_grid     = new_grid,
    method       = method,
    mask         = mask,
    new_mask     = new_mask,
    weights      = weights,
    keep_weights = keep_weights
  )

}

#' @rdname geo_opts
#' @inheritParams geo_transform
#' @export
geo_opts_subgrid <- function(i1, i2, j1, j2) {
  list(i1 = i1, i2 = i2, j1 = j1, j2 = j2)
}

#' @rdname geo_opts
#' @inheritParams geo_transform
#' @export
geo_opts_zoom <- function(centre_lon, centre_lat, length_x, length_y) {
  list(
    centre_lon = centre_lon, centre_lat = centre_lat,
    length_x = length_x, length_y = length_y
  )
}

#' @rdname geo_opts
#' @inheritParams geo_transform
#' @export
geo_opts_xsection <- function(
    p1,
    p2,
    n            = 100,
    method       = c("bilinear", "nearest", "bicubic"),
    weights      = NULL,
    keep_weights = FALSE
) {

  method <- match.arg(method)

  list(
    p1           = p1,
    p2           = p2,
    n            = n,
    method       = method,
    weights      = weights,
    keep_weights = keep_weights
  )
}

#' @rdname geo_opts
#' @inheritParams geo_transform
#' @export
geo_opts_upscale <- function(
  factor,
  method              = "mean",
  downsample_location = "bottom_left"
) {
  list(
    factor              = factor,
    method              = method,
    downsample_location = downsample_location
  )
}

# meteogrid functions do not recognise "longlat" as a longlat projection
# Change to "latlong" to get the projection recognised.
latlong_fudge <- function(geo) {
  if (meteogrid::is.geofield(geo)) {
    if (attr(geo, "domain")[["projection"]][["proj"]] == "longlat") {
      attr(geo, "domain")[["projection"]][["proj"]] <- "latlong"
    }
  }

  if (meteogrid::is.geodomain(geo)) {
    if (geo[["projection"]][["proj"]] == "longlat") {
      geo[["projection"]][["proj"]] <- "latlong"
    }
  }

  geo
}

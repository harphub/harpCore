#######################
# Point interpolation #
#######################

#' Title
#'
#' @param x
#' @param points
#' @param method
#' @param mask
#' @param force
#'
#' @return
#' @export
#'
#' @examples
geo_weights_points <- function(
  x,
  points,
  method     = c("bilinear", "nearest", "bicubic"),
  mask       = NULL,
  pointmask = NULL,
  force      = FALSE
) {
  UseMethod("geo_weights_points")
}

#' @export
geo_weights_points.geodomain <- function(
  x,
  points,
  method = c("bilinear", "nearest", "bicubic"),
  mask   = NULL,
  force  = FALSE
) {

  if (missing(points)) {
    message("Using default points from `station_list`.")
    points <- get("station_list")
  }
  method <- match.arg(method)
  ll_cols <- c("lon", "lat")
  if (
    !is.data.frame(points) ||
      all(intersect(ll_cols, colnames(points)) != ll_cols)
  ) {
    stop(
      "points must be a data frame including columns 'lon' and 'lat'."
    )
  }

  if (!x[["projection"]][["proj"]] %in% c("longlat", "latlong", "lalo")) {
    points <- cbind(
      points,
      meteogrid::project(points[c("lon", "lat")], proj = x[["projection"]])
    )
  }

  res <- dplyr::bind_cols(
    points,
    point_data = suppressWarnings(meteogrid::point.interp.init(
      x,
      lon       = points[["lon"]],
      lat       = points[["lat"]],
      method    = method,
      mask      = mask,
      pointmask = points[["mask"]],
      force     = force
    ))
  )

  res <- res[stats::complete.cases(res), ]
  attr(res, "domain") <- x
  res
}

#' @export
geo_weights_points.geofield <- function(
  x,
  points,
  method = c("bilinear", "nearest", "bicubic"),
  mask   = NULL,
  force  = FALSE
) {

  if (missing(points)) {
    message("Using default points from `station_list`.")
    points <- get("station_list")
  }

  method <- match.arg(method)
  x <- meteogrid::as.geodomain(x)

  geo_weights_points(x, points, method, mask, force)

}

#' @export
geo_weights_points.harp_geolist <- function(
  x,
  points,
  method = c("bilinear", "nearest", "bicubic"),
  mask   = NULL,
  force  = FALSE
) {

  if (missing(points)) {
    message("Using default points from `station_list`.")
    points <- get("station_list")
  }

  method <- match.arg(method)
  dom <- get_domain(x)

  geo_weights_points(x, points, method, mask, force)
}

#############################
# Interpolation to new grid #
#############################

#' Title
#'
#' @param x
#' @param new_grid
#' @param method
#' @param mask
#' @param new_mask
#'
#' @return
#' @export
#'
#' @examples
geo_weights_regrid <- function(
  x,
  new_grid,
  method   = c("bilinear", "nearest", "bicubic"),
  mask     = NULL,
  new_mask = NULL
) {
  UseMethod("geo_weights_regrid")
}

geo_weights_regrid_geo <- function(
    x,
    new_grid,
    method   = c("bilinear", "nearest", "bicubic"),
    mask     = NULL,
    new_mask = NULL
) {


  method  <- match.arg(method)
  dom     <- get_domain(x)
  new_dom <- get_domain(new_grid)

  meteogrid::regrid.init(dom, new_dom, method, mask, new_mask)
}

#' @export
geo_weights_regrid.geofield <- function(
    x,
    new_grid,
    method   = c("bilinear", "nearest", "bicubic"),
    mask     = NULL,
    new_mask = NULL
) {
  check_args_geo_regrid(x, new_grid, mask, new_mask)
  method <- match.arg(method)
  geo_weights_regrid_geo(x, new_grid, method, mask, new_mask)
}

#' @export
geo_weights_regrid.geodomain <- function(
    x,
    new_grid,
    method   = c("bilinear", "nearest", "bicubic"),
    mask     = NULL,
    new_mask = NULL
) {
  check_args_geo_regrid(x, new_grid, mask, new_mask)
  method <- match.arg(method)
  geo_weights_regrid_geo(x, new_grid, method, mask, new_mask)
}

#####################################################
# Cross section of a grid (one vertical level only) #
#####################################################

#' Title
#'
#' @param x
#' @param p1
#' @param b
#' @param n
#' @param method
#'
#' @return
#' @export
#'
#' @examples
geo_weights_xsection <- function(
    x,
    p1,
    p2,
    n            = 100,
    method       = c("bilinear", "nearest", "bicubic")
) {
  UseMethod("geo_weights_xsection")
}

#' @export
geo_weights_xsection.geodomain <- function(
    x,
    p1,
    p2,
    n            = 100,
    method       = c("bilinear", "nearest", "bicubic")
) {

  invisible(lapply(list(p1, p2, n), check_numeric))

  p1 <- correct_length(p1, "p1", 2)
  p2 <- correct_length(p2, "p2", 2)
  n  <- correct_length(round_to_integer(n, "n"), "n", 1)

  in_domain(p1, x, "p1")
  in_domain(p2, x, "p2")

  points <- xsection_points(x[["projection"]], p1, p2, n)
  geo_weights_points(x, points, method)

}

#' @export
geo_weights_xsection.geofield <- function(
    x,
    p1,
    p2,
    n            = 100,
    method       = c("bilinear", "nearest", "bicubic")
) {

  method <- match.arg(method)
  geo_weights_xsection(get_domain(x), p1, p2, n, method)

}

# xsection helper functions
in_domain <- function(p, dom, arg, caller = rlang::caller_env()) {
  indices <- as.numeric(meteogrid::point.index(dom, p[1], p[2], clip = FALSE))
  if (any(indices < 1) || any(c(indices[1] > dom$nx, indices[2] > dom$ny))) {
    value <- paste(p, collapse = ", ")
    index <- paste(sprintf("%.2f", indices), collapse = ", ")
    cli::cli_abort(c(
      "Cross section end point outside of domain",
      "x" = "{.arg {arg}} = ({value}) has indices ({index})",
      "i" = "Domain dimensions are 1:{dom$nx} x 1:{dom$ny}"
    ))
  }
}

xsection_points <- function(proj_list, a, b, n) {
  a <- meteogrid::project(a, proj = proj_list)
  b <- meteogrid::project(b, proj = proj_list)
  if (proj_list[["proj"]] %in% c("latlong", "longlat", "lalo")) {
    points <- great_circle_points(a, b, n, proj_list[["R"]])
  } else {
    points <- projected_points(a, b, n)
  }
  res <- cbind(
    meteogrid::project(points, proj = proj_list, inv = TRUE),
    data.frame(distance = points[, 3])
  )
  colnames(res) <- c("lon", "lat", "distance")
  res
}

projected_points <- function(a, b, n) {
  reverse <- FALSE
  if (a[["x"]] > b[["x"]]) {
    reverse <- TRUE
    atmp    <- a
    a       <- b
    b       <- atmp
  }
  theta <- atan((b[["y"]] - a[["y"]]) / (b[["x"]] - a[["x"]]))
  hyp   <- sqrt((b[["y"]] - a[["y"]]) ^ 2 + (b[["x"]] - a[["x"]]) ^ 2)
  dist  <- seq(0, hyp, length.out = n)
  x     <- a[["x"]] + cos(theta) * dist
  y     <- a[["y"]] + sin(theta) * dist

  if (reverse) {
    x <- rev(x)
    y <- rev(y)
  }

  matrix(c(x, y, dist), ncol = 3)
}

great_circle_points <- function(a, b, n, R) {

  # n shouldn't include start and end points
  n <- n - 2

  to_rad <- pi / 180

  lon1 <- a[["x"]] * to_rad
  lat1 <- a[["y"]] * to_rad
  lon2 <- b[["x"]] * to_rad
  lat2 <- b[["y"]] * to_rad

  d <- acos(
    sin(lat1) * sin(lat2) + cos(lat1) * cos(lat2) * cos(lon1 - lon2)
  )

  n <- max(round(n), 1)
  f <- 1:n / (n + 1)
  A <- sin((1 - f) * d ) / sin(d)
  B <- sin(f * d) / sin(d)
  x <- A * cos(lat1) * cos(lon1) + B * cos(lat2) * cos(lon2)
  y <- A * cos(lat1) * sin(lon1) + B * cos(lat2) * sin(lon2)
  z <- A * sin(lat1) + B * sin(lat2)

  lat <- atan2(z, sqrt(x ^ 2 + y ^ 2))
  lon <- atan2(y, x)

  res <- cbind(lon, lat) / to_rad
  unname(cbind(
    rbind(unname(as.matrix(a)), res, unname(as.matrix(b))),
    cumsum(c(0, rep(d * R / (n + 1), (n + 1))))
  ))
}



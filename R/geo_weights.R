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
  method = "bilinear",
  mask   = NULL,
  force  = FALSE
) {
  UseMethod("geo_weights_points")
}

#' @export
geo_weights_points.geodomain <- function(
  x,
  points,
  method = "bilinear",
  mask   = NULL,
  force  = FALSE
) {

  ll_cols <- c("lon", "lat")
  if (
    !is.data.frame(points) ||
      all(intersect(ll_cols, colnames(points)) != ll_cols)
  ) {
    stop(
      "points must be a data frame including columns 'lon' and 'lat'."
    )
  }

  x <- dplyr::bind_cols(
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
  x <- x[complete.cases(x), ]
}

#' @export
geo_weights_points.geofield <- function(
  x,
  points,
  method = "bilinear",
  mask   = NULL,
  force  = FALSE
) {

  x <- meteogrid::as.geodomain(x)

  geo_weights_points(x, points, method, mask, force)

}

#' @export
geo_weights_points.geolist <- function(
  x,
  points,
  method = "bilinear",
  mask   = NULL,
  force  = FALSE
) {

  x <- meteogrid::as.geodomain(x[[1]])

  geo_weights_points(x, points, method, mask, force)
}

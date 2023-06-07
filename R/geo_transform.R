#' Title
#'
#' @param x
#' @param points
#' @param method
#' @param mask
#' @param force
#' @param weights
#' @param keep_weights
#'
#' @return
#' @export
#'
#' @examples
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

geo_points.geofield <- function(
  x,
  points,
  method       = c("bilinear", "nearest", "bicubic"),
  mask         = NULL,
  force        = FALSE,
  weights      = NULL,
  keep_weights = FALSE
) {
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
      stop(
        "points must be a data frame including columns 'lon' and 'lat'."
      )
    }
    options(warn = -1)
    x <- dplyr::mutate(
      points,
      point_data = suppressWarnings(meteogrid::point.interp(
        x,
        lon       = points[["lon"]],
        lat       = points[["lat"]],
        method    = method,
        mask      = mask,
        pointmask = points[["mask"]],
        force     = force
      ))
    )
    options(warn = 0)

  } else {

    options(warn = -1)
    x <- dplyr::mutate(
      points,
      point_data = suppressWarnings(
        meteogrid::point.interp(x, weights = weights)
      )
    )
    options(warn = 0)
  }
  x[complete.cases(x), ]
}

geo_points.geolist <- function(
  x,
  points,
  method       = c("bilinear", "nearest", "bicubic"),
  mask         = NULL,
  force        = FALSE,
  weights      = NULL,
  keep_weights = FALSE
) {
  if (is.null(weights)) {
    weights <- geo_weights_points(x[[1]], points, method, mask, force)
  }
  #dplyr::bind_rows(
    lapply(x, geo_points, points, weights = weights)
  #)
}

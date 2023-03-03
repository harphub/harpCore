# neighbourhood methods
#
#' Smooth a 2d field based an a neighbourhood radius
#'
#' \code{nbhd_smooth} takes a 2d field, and for each pixel in that field a
#' square neighbourhood is constructed with a specified radius - that is the
#' number of pixels to the left/right and above/below the pixel at the
#' neighbourhood centre. For each pixel, the mean value of the pixels in the
#' neighbourhood is returned resulting in a smoothed field on the same grid as
#' the original field.
#'
#' For pixels that are closer to the edge of the input field than the radius
#' there are two options. The default is to zero pad the field outwards with
#' pixels outside of the original field set to zero. If
#' \code{boundary = "missing"} the pixels closer to the edge than the radius
#' will be set to missing.
#'
#' The neighbourhood means are computed by first calculating the cumulative sum
#' of the array from the top left to the bottom right and then computing the
#' total for each neighbourhood by subtraction. This means that the algorithm
#' is equally efficient regardless of the neighbourhood size. It is possible
#' to compute each stage at a time with fist \code{cumsum_2d} and then
#' \code{nbhd_smooth_cumsum}.
#'
#' If a threshold is given, the binary probability of the input field compared
#' with the threshold is computed at each pixel before applying the function.
#' By default the comparison is \code{x >= threshold}, but other comparisons
#' can be chosen using the \code{comparator} argument.
#'
#' @param x A 2d array, geofield or geolist.
#' @param radius The radius of the neighbourhood in pixels.
#' @param threshold A threshold for computing binary probabilities. If
#' \code{comparator = "between"}, it must be a two element vector. Set to NA
#'   to use the raw data.
#' @param comparator How to compare \code{x} with the threshold to compute
#'   binary probabilities. Can be \code{"ge"}, \code{"gt"}, \code{"le"}, or
#'   \code{"lt"} for >=, >, <= and < respectively. Can also be
#'   \code{"between"} or \code{"outside"}, in which case the binary probability
#'   of being between or outside of the two values given in \code{threshold} is
#'   computed.
#' @param include_low Logical. Whether to include to the lower of the two
#'   thresholds in the comparison when \code{comparator = "between"} or
#'   \code{comparator = "outside"}.
#' @param include_high Logical. Whether to include to the higher of the two
#'   thresholds in the comparison when \code{comparator = "between"} or
#'   \code{comparator = "outside"}.
#' @param boundary How to treat the boundaries. Current options are
#'   \code{"zero_pad"} and \code{"missing"}. See Details for more information
#'   on these options.
#'
#' @return A smoothed 2d field. In the case of geofields and geolists, the
#'   attributes are retained.
#' @export
#'
#' @examples
#' # Create a 2d array
#' z <- array(dim = c(100, 100))
#' for (i in 1:100) {
#'   for (j in 1:100) {
#'     z[i, j] <- sin(i / 10) + sin(j / 10)
#'  }
#' }
#'
#' image(z, zlim = range(z), col = heat.colors(255))
#' # With zero-padding the input array
#' image(nbhd_smooth(z, 5), zlim = range(z), col = heat.colors(255))
#' image(nbhd_smooth(z, 10), zlim = range(z), col = heat.colors(255))
#' image(nbhd_smooth(z, 20), zlim = range(z), col = heat.colors(255))
#'
#' # Without zero padding
#' image(nbhd_smooth(z, 5, boundary = "missing"), zlim = range(z), col = heat.colors(255))
#' image(nbhd_smooth(z, 10, boundary = "missing"), zlim = range(z), col = heat.colors(255))
#'
#' # Add a threshold
#' image(nbhd_smooth(z, 10, 0.5), zlim = c(0, 1), col = heat.colors(255))
#' image(nbhd_smooth(z, 10, 0.5, "lt"), zlim = c(0, 1), col = heat.colors(255))
#' image(nbhd_smooth(z, 10, c(-0.5, 0.5), "between"), zlim = c(0, 1), col = heat.colors(255))
#' image(nbhd_smooth(z, 10, c(-0.5, 0.5), "outside"), zlim = c(0, 1), col = heat.colors(255))
nbhd_smooth <- function(
  x,
  radius,
  threshold    = NA,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  boundary     = c("zero_pad", "missing")
) {
  UseMethod("nbhd_smooth")
}

#' @export
nbhd_smooth.geofield <- function(
  x,
  radius,
  threshold    = NA,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  boundary     = c("zero_pad", "missing")
) {

  comparator <- match.arg(comparator)
  if (comparator %in% c("between", "outside") && length(threshold) != 2) {
    stop(
      "For comparator = 'between' threshold must be a vector of length 2."
    )
    threshold = sort(threshold)
  }
  if (!comparator %in% c("between", "outside") && length(threshold) != 1) {
    warning(
      "Only the first element of threshold (", threshold[1], ") will be used."
    )
  }
  boundary <- match.arg(boundary)

  attrs <- attributes(x)
  x <- cpp_nbhd_smooth(
    x,
    radius,
    threshold,
    comparator,
    include_low,
    include_high,
    boundary
  )
  attributes(x) <- attrs
  x
}

#' @export
nbhd_smooth.array <- function(
  x,
  radius,
  threshold    = NA,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  boundary     = c("zero_pad", "missing")
) {

  comparator <- match.arg(comparator)
  if (comparator %in% c("between", "outside") && length(threshold) != 2) {
    stop(
      "For comparator = 'between' threshold must be a vector of length 2."
    )
    threshold = sort(threshold)
  }
  if (!comparator %in% c("between", "outside") && length(threshold) != 1) {
    warning(
      "Only the first element of threshold (", threshold[1], ") will be used."
    )
  }
  if (!length(dim(x)) == 2) {
    stop("`x` must be a 2-d array")
  }
  boundary <- match.arg(boundary)

  cpp_nbhd_smooth(
    x,
    radius,
    threshold,
    comparator,
    include_low,
    include_high,
    boundary
  )
}

#' @export
nbhd_smooth.geolist <- function(
  x,
  radius,
  threshold    = NA,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE,
  boundary     = c("zero_pad", "missing")
) {
  as_geolist(
    lapply(
      x,
      cpp_nbhd_smooth,
      radius,
      threshold,
      comparator,
      include_low,
      include_high,
      boundary
    )
  )
}

#' @rdname nbhd_smooth
#' @export
cumsum_2d <- function(
  x,
  threshold    = NA,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE
) {
  UseMethod("cumsum_2d")
}

#' @export
cumsum_2d.geofield <- function(
  x,
  threshold    = NA,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE
) {

  comparator <- match.arg(comparator)
  if (comparator %in% c("between", "outside") && length(threshold) != 2) {
    stop(
      "For comparator = 'between' threshold must be a vector of length 2."
    )
    threshold = sort(threshold)
  }
  if (!comparator %in% c("between", "outside") && length(threshold) != 1) {
    warning(
      "Only the first element of threshold (", threshold[1], ") will be used."
    )
  }

  attrs <- attributes(x)
  x <- cpp_cumsum2d(x, threshold, comparator, include_low, include_high)
  attributes(x) <- attrs
  x
}

#' @export
cumsum_2d.array <- function(
  x,
  threshold    = NA,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE
) {

  comparator <- match.arg(comparator)
  if (comparator %in% c("between", "outside") && length(threshold) != 2) {
    stop(
      "For comparator = 'between' threshold must be a vector of length 2."
    )
    threshold = sort(threshold)
  }
  if (!comparator %in% c("between", "outside") && length(threshold) != 1) {
    warning(
      "Only the first element of threshold (", threshold[1], ") will be used."
    )
  }
  if (!length(dim(x)) == 2) {
    stop("`x` must be a 2-d array")
  }

  cpp_cumsum2d(x, threshold, comparator, include_low, include_high)
}

#' @export
cumsum_2d.geolist <- function(
  x,
  threshold    = NA,
  comparator   = c("ge", "gt", "le", "lt", "between", "outside"),
  include_low  = TRUE,
  include_high = TRUE
) {
  as_geolist(
    lapply(
      x,
      cpp_cumsum2d,
      threshold,
      comparator,
      include_low,
      include_high
    )
  )
}

#' @rdname nbhd_smooth
#' @export
nbhd_smooth_cumsum <- function(
    x, radius, boundary = c("zero_pad", "missing")
) {
  UseMethod("nbhd_smooth_cumsum")
}

#' @export
nbhd_smooth_cumsum.geofield <- function(
    x, radius, boundary = c("zero_pad", "missing")
) {
  boundary <- match.arg(boundary)
  attrs <- attributes(x)
  x <- cpp_nbhd_smooth_cumsum(x, radius, boundary)
  attributes(x) <- attrs
  x
}

#' @export
nbhd_smooth_cumsum.array <- function(
    x, radius, boundary = c("zero_pad", "missing")
) {
  if (!length(dim(x)) == 2) {
    stop("`x` must be a 2-d array")
  }
  boundary <- match.arg(boundary)
  cpp_nbhd_smooth_cumsum(x, radius, boundary)
}

#' @export
nbhd_smooth_cumsum.geolist <- function(
    x, radius, boundary = c("zero_pad", "missing")
) {
  as_geolist(lapply(x, cpp_nbhd_smooth_cumsum, radius, boundary))
}

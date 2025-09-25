#' Circular statistics
#'
#' These functions compute statistics for circular data. They would typically
#' be used when calculating statistics from wind direction. If `x` is a matrix
#' or a data frame, the statistics are computed for each row in the data.
#'
#' Note that `circle_var()` gives a dimensionless output between 0 and 1,
#' whereas `circle_sd()` and `circle_sd_sq()` can be interpreted as being in
#' degrees (or radians depending on what units the data are in), or the square.
#'
#' @param x,y A numeric vector, matrix, or data frame in degrees or radians.
#' @param degrees Logical. Set to `TRUE` for x (and y) in degrees (the default).
#'   If set to FALSE x (and y) will be assumed to be in radians.
#'
#' @return A numeric value or vector in the same units as the input.
#'
#' @name circle_stats
NULL

#' @rdname circle_stats
#' @export
#'
#' @examples
#' # Doesn't cross zero
#' circle_bias(180, 90)
#' #
#' # Crosses zero
#' circle_bias(355, 5)
#' #
#' # Crosses zero the other way
#' circle_bias(5, 355)
circle_bias <- function(x, y, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
    y <- deg_to_rad(y)
  }
  res <- atan2(sin(x - y), cos(x - y))
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

#' @rdname circle_stats
#' @export
#'
#' @examples
#' # All in the 0 - 180 degrees range
#' circle_mean(c(20, 40, 60, 120, 140))
#' #
#' # Crossing zero
#' circle_mean(c(340, 30, 40, 320, 275))
circle_mean <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  mean_func <- get_mean_func(x)
  res <- atan2(mean_func(sin(x)), mean_func(cos(x)))
  res[res < 0] <- res[res < 0] + 2 * pi
  res[res == 2 * pi] <- 0
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

#' @rdname circle_stats
#' @export
#'
#' @examples
#' # Clustered angles
#' circle_var(c(340, 10, 350, 5, 2, 355, 352, 8))
#' #
#' # Spread angles
#' circle_var(c(0, 90, 180, 270))
circle_var <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  mean_func <- get_mean_func(x)
  res <- 1 - sqrt(mean_func(cos(x)) ^ 2 + mean_func(sin(x)) ^ 2)
  res
}

#' @rdname circle_stats
#' @export
#'
#' @examples
#' # Clustered angles
#' circle_sd(c(340, 10, 350, 5, 2, 355, 352, 8))
#' #
#' # Spread angles
#' circle_sd(c(0, 90, 180, 270))
circle_sd <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  mean_func <- get_mean_func(x)
  R <- sqrt(mean_func(cos(x)) ^ 2 + mean_func(sin(x)) ^ 2)
  res <- sqrt(-2 * log(R))
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

#' @rdname circle_stats
#' @export
#'
#' @examples
#' # Clustered angles
#' circle_sd_sq(c(340, 10, 350, 5, 2, 355, 352, 8))
#' #
#' # Spread angles
#' circle_sd_sq(c(0, 90, 180, 270))
circle_sd_sq <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  mean_func <- get_mean_func(x)
  R <- sqrt(mean_func(cos(x)) ^ 2 + mean_func(sin(x)) ^ 2)
  res <- -2 * log(R)
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

# This function assumes that x is the squared circular SD. To avoid confusion
# it should only be used internally.
circle_spread <- function(x, degrees = TRUE) {
  if (degrees) {
    x <- deg_to_rad(x)
  }
  res <- sqrt(mean(x))
  if (degrees) {
    return(rad_to_deg(res))
  }
  res
}

# The skill is essentially the standard deviation of the error.
circle_skill <- function(x, degrees = TRUE) {
  circle_sd(x, degrees)
}

deg_to_rad <- function(x) {
  x * pi / 180
}

rad_to_deg <- function(x) {
  x * 180 / pi
}

get_mean_func <- function(x) {
  if (is.data.frame(x) || is.matrix(x)) {
    return(rowMeans)
  }
  mean
}

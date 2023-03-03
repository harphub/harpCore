# Meteorological functions

#' Compute dewpoint temperature from temperature and relative humidity
#'
#' The computation of dewpoint temperature is done using the formula
#' \deqn{\frac{b\left(ln(RH) + \left(\frac{aT}{(b+T)}\right)\right)}{\left(a-\left(ln(RH) + \left(\frac{aT}{(b+T)}\right)\right)\right)}}
#' And the computation for relative humidity is done following
#' \deqn{RH=\frac{e}{e_s}}
#' where
#' \deqn{e=\frac{QP}{0.378Q+0.622}}
#' \deqn{e_s=6.112e^{\frac{aT}{b+T}}}
#' In the above formulae:\cr
#' \eqn{T} is the temperature in \eqn{\degree C} \cr
#' \eqn{RH} is the relative humidity expressed as a decimal between 0 and 1 \cr
#' \eqn{Q} is the specific humidity in \eqn{kg.kg^{-1}} \cr
#' \eqn{P} is the atmospheric pressure in \eqn{hPa} \cr
#' \eqn{a} and \eqn{b} are tuning parameters \cr
#' \cr
#' The default values for \eqn{a} and \eqn{b} are 17.67 and 243.5 respectively as
#' recommended by NOAA.
#'
#' Other tunings for \eqn{a} and \eqn{b} are possible:
#'
#' \eqn{a=17.27}, \eqn{b=237.3} is valid for \eqn{0\lt T \lt 60}
#'
#' Alternatively:
#'
#' \eqn{a=17.62}, \eqn{b=243.12}\cr
#' \eqn{a=17.625}, \eqn{b=243.04}
#'
#' Finally A.L. Buck (1981) recommends different tunings based on
#' temperature:
#'
#' \eqn{a=17.368}, \eqn{b=238.88} for \eqn{T \gt 0}\cr
#' \eqn{a=17.966}, \eqn{b=247.15} for \eqn{T \lt 0}\cr
#'
#' Note that the formulation used for dewpoint temperature does not take
#' atmospheric pressure into account, so may not be correct at higher
#' altitudes.
#'
#' @name met_functions
NULL

#' @rdname met_functions
#' @param rh A vector of relative humidities expressed as decimal fractions
#'   between 0 and 1. If the maximum is greater than 5, it is assumed that all
#'   relative humidities are in \eqn{\%} and will be divided by 100.
#' @param t A vector of the same length as \code{rh} temperatures in
#'   \eqn{\degree C}. If the maximum is greater than 200, it is assumed that
#'   all temperatures are in \eqn{Kelvin} and they will be converted to to
#'   \eqn{\degree C}.
#' @param a A tuning parameter. The default value is \eqn{17.67}.
#' @param b A tuning parameter. The default value is \eqn{243.5}.
#'
#' @return A numeric vector of the same length as \code{t}.
#' @export
#'
#' @examples
#' rh_to_td(0.8, 20)
#'
#' # In Kelvin and %
#' rh_to_td(80, 293.15)
rh_to_td <- function(rh, t, a = 17.67, b = 243.5) {

  if (length(rh) != length(t)) {
    stop("`t` must be the same length as `rh`.")
  }

  # Get temperature in degC and RH as 0 - 1.
  if (max(rh) > 5) {
    warning("rh assumed to be in %. Coverting to 0-1.")
    rh <- rh / 100
  }
  if (max(t) > 200) {
    warning("t assumed to be in K. Converting to degrees C.")
    t <- t - 273.15
  }
  # RH cannot be 0 due to taking log, and max rh must be 1.
  min_rh <- 1e-3
  rh[rh < min_rh] <- min_rh
  rh[rh > 1] <- 1

  common_val <- log(rh) + ((a * t) / (b + t))
  b * common_val / (a - common_val)
}

#' Compute relative humidity from specific humidity and temperature
#'
#' @rdname met_functions
#' @param q The specific humidity in \eqn{kg.kg^{-1}}
#' @param p The air pressure in \eqn{hPa}
#'
#' @export
#'
#' @examples
#' #
#' # Relative humidity is returned as a decimal fraction between 0 and 1
#' q_to_rh(0.013, 20)
q_to_rh <- function(q, t, p = 1013.25, a = 17.67, b = 243.5) {
  if (length(q) != length(t)) {
    stop("`t` must be the same length as `q`.")
  }
  if (max(t) > 200) {
    warning("t assumed to be in K. Converting to degrees C.")
    t <- t - 273.15
  }
  if (max(q) > 0.05) {
    warning("q assumed to be in g/kg. Converting to kg/kg.")
    t <- t - 273.15
  }
  if (p > 2000) {
    warning("p assumed to be in Pa. Converting to hPa.")
    p <- p / 100
  }
  es <- 6.112 * exp((a * t) / (t + b))
  e  <- q * p / (0.378 * q + 0.622)
  rh <- e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0
  rh
}

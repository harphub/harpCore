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

#' Compute virtual temperature
#'
#' The virtual temperature is computed from the air temperature and the
#' specific humidity of the air using the formula `Tv = T(1 + 0.61Q)`. If the
#' function suspects that the inputs are in the wrong units (degrees C instead
#' of Kelvin; g/kg instead of kg/kg), they are converted to the correct units
#' with a warning.
#'
#' @param temp Air temperature in K.
#' @param spec_hum Specific humidity in kg/kg.
#'
#' @returns Virtual temperature in K.
#' @export
#'
#' @examples
#' virtual_temp(293.15, 0.01)
#'
#' # Corrections to inputs made if in wrong units
#' virtual_temp(20, 10)
virtual_temp <- function(temp, spec_hum) {
  temp_test <- mean(temp, na.rm = TRUE)
  if (is_geolist(temp_test)) {
    temp_test <- mean(geofield(temp_test), na.rm = TRUE)
  }
  if (temp_test < 100) {
    txt <- "You supplied `temp`"
    if (length(temp) > 1) {
      txt <- paste(txt, "with mean")
    }
    cli::cli_warn(c(
      "{.arg temp} suspected to be in degrees C.",
      "i" = "{txt} = {temp_test}. Converted to Kelvin."
    ))
    temp <- temp + 273.15
  }
  spec_hum_test <- mean(spec_hum, na.rm = TRUE)
  if (is_geolist(spec_hum_test)) {
    spec_hum_test <- mean(geofield(spec_hum_test), na.rm = TRUE)
  }
  if (spec_hum_test > 0.1) {
    txt <- "You supplied `spec_hum`"
    if (length(spec_hum) > 1) {
      txt <- paste(txt, "with mean")
    }
    cli::cli_warn(c(
      "{.arg spec_hum} suspected to be in g/kg.",
      "i" = "{txt} = {temp_test}. Converted to kg/kg."
    ))
    spec_hum <- spec_hum / 1000
  }
  temp * (1 + 0.61 * spec_hum)
}

#' Potential temperature
#'
#' @description
#' Compute the potential temperature of an air parcel - that is the temperature
#' it would have if it were brought adiabatically to a reference temperature.
#'
#' The potential temperature is defined as:
#' \deqn{ \theta = T \left( \frac{p_0}{p} \right)^{R/c_p} }
#'
#' where \eqn{\theta} is potential temperature, \eqn{T} is temperature,
#' \eqn{p} is pressure, \eqn{p_0} is reference pressure, \eqn{R} is the
#' gas constant for dry air, and \eqn{c_p} is the specific heat at
#' constant pressure.
#'
#' @param temp Temperature (K)
#' @param press Air parcel pressure (Pa)
#' @param press_ref Reference pressure (Pa). The default is 100000 Pa.
#'
#' @returns Potential temperature (K)
#' @export
#'
#' @examples
#' potential_temp(280, 85000)
potential_temp <- function(temp, press, press_ref = 100000) {
  UseMethod("potential_temp")
}

#' @export
potential_temp.default <- function(temp, press, press_ref = 100000) {
  temp * (press_ref / press) ^ 0.286
}

#' @export
potential_temp.harp_xs <- function(temp, press, press_ref = 100000) {
  press_col <- intersect(c("value", "pressure"), colnames(press))
  temp$value <- potential_temp(temp$value, press[[press_col]], press_ref)
  temp
}

#' @export
potential_temp.harp_xs_list <- function(temp, press, press_ref = 100000) {
  structure(
    mapply(
      potential_temp, temp, press, MoreArgs = list(press_ref = press_ref),
      SIMPLIFY = FALSE
    ),
    class = class(temp)
  )
}

#' Compute the vorticity or divergence from vector components
#'
#' These functions compute the vorticity and divergence of a vector field. If
#' the vectors are relative to the earth but the grid is on a projection, the
#' vectors can optionally be rotated to the projected grid prior to computation.
#'
#' @param u A geofield or geolist of the u component
#' @param v A geofield or geolist of the v component
#' @param rotate_to_grid Logical. Whether to rotate the vector components from
#'   earth relative to grid relative. Default is FALSE
#' @param rotation_init If the angles for rotation have been previously
#'   computed (e.g. using `meteogrid::geowind.init`) they can be passed here.
#'   Default is NULL
#'
#' @return A geofield if `u` and `v` are geofields or a geolist if `u` and `v`
#'   are geolists.
#' @export
#'
vorticity <- function(u, v, rotate_to_grid = FALSE, rotation_init = NULL) {
  UseMethod("vorticity")
}

#' @importFrom zeallot "%<-%"
#' @export
vorticity.geofield <- function(
    u, v, rotate_to_grid = FALSE, rotation_init = NULL
) {

  check_uv_domains(u, v)

  if (rotate_to_grid) {
    c(u, v) %<-% meteogrid::geowind(u, v, inv = TRUE, init = rotation_init)
  }

  attrs <- attributes(u)

  dudy <- first_derivative(u, "y")
  dvdx <- first_derivative(v, "x")

  res <- dvdx - dudy
  attributes(res) <- attrs
  attr(res, "info")$name = "Vorticity"
  res
}

#' @export
vorticity.harp_geolist <- function(
    u, v, rotate_to_grid = FALSE, rotation_init = NULL
) {

  check_uv_domains(u, v)

  if (rotate_to_grid && is.null(rotation_init)) {
    rotation_init <- meteogrid::geowind.init(get_domain(u))
  }

  glapply2(u, v, vorticity, rotate_to_grid, rotation_init)
}

#' @export
#' @rdname vorticity
divergence <- function(u, v, rotate_to_grid = FALSE, rotation_init = NULL) {
  UseMethod("divergence")
}

#' @export
divergence.geofield <- function(
    u, v, rotate_to_grid = FALSE, rotation_init = NULL
) {

  check_uv_domains(u, v)

  if (rotate_to_grid) {
    c(u, v) %<-% meteogrid::geowind(u, v, inv = TRUE, rotation_init)
  }

  attrs <- attributes(u)

  dudx <- first_derivative(u, "x")
  dvdy <- first_derivative(v, "y")

  res <- dudx + dvdy
  attributes(res) <- attrs
  attr(res, "info")$name = "Divergence"
  res
}

#' @export
divergence.harp_geolist <- function(
    u, v, rotate_to_grid = FALSE, rotation_init = NULL
) {

  check_uv_domains(u, v)

  if (rotate_to_grid && is.null(rotation_init)) {
    rotation_init <- meteogrid::geowind.init(get_domain(u))
  }

  glapply2(u, v, divergence, rotate_to_grid, rotation_init)
}


check_uv_domains <- function(u, v) {

  if (!identical(sort(class(u)), sort(class(v)))) {
    cli::cli_abort(c(
      "{.arg u} and {.arg v} are not the same class.",
      "x" = "You supplied {.arg u} {.cls {class(u)}}, {.arg v} {.cls {class(v)}}.",
      "i" = paste(
        "If {.arg u} has class {.cls {class(u)}}",
        "{.arg v} must also have class {.cls {class(u)}}."
      )
    ), call = rlang::caller_env())
  }

  dom <- get_domain(u)
  if (!check_same_domain(list(dom, get_domain(v)))) {
    rlang::abort(
      "geofields must be on the same domain.", call = rlang::caller_env()
    )
  }

  ll_proj <- c(
    "lalo","longlat", "latlong", "rot_longlat", "rot_latlong", "RotLatLon"
  )
  if (dom$projection$proj %in% ll_proj) {
    rlang::abort("Currently only works on projected geofields")
  }

}

# Compute the first derivative of a geofield in the specified direction - for
# internal points, centred differencing is used, and for points on the edge,
# forward or backward differencing is used depending on which edge it is.
first_derivative <- function(x, direction = c("x", "y")) {

  spacing <- get_domain(x)[[paste0("d", direction)]]

  attributes(x) <- attributes(x)[names(attributes(x)) == "dim"]

  # diff only works on individual columns so transpose for y direction
  if (direction == "y") {
    x <- t(x)
  }

  # Allocate an array for the result
  res <- array(dim = dim(x))

  # Centred difference points
  res[2:(nrow(x) - 1), ] <- diff(x, lag = 2) / (2 * spacing)

  # Forward difference at the left edge
  res[1,] <- (x[2,] - x[1,]) / spacing

  # Backward difference at the right edge
  res[nrow(x), ] <- (x[nrow(x), ] - x[(nrow(x) - 1), ]) / spacing

  if (direction == "y") {
    res <- t(res)
  }

  res
}

#' Convert geopotential to height
#'
#' A simple conversion of geopotential in m^2 / s^2 to height in m, by dividing
#' by acceleration due to gravity (g = 9.80665).
#'
#' @param x
#'
#' @returns The same class as x
#' @export
geopotential_to_height <- function(x) {
  UseMethod("geopotential_to_height")
}

#' @export
geopotential_to_height.default <- function(x) {
  x / 9.80665
}

#' @export
geopotential_to_height.harp_xs <- function(x) {
  x$value <- x$value / 9.80665
  x
}

#' @export
geopotential_to_height.harp_xs_list <- function(x) {
  structure(
    lapply(x, geopotential_to_height),
    class = class(x)
  )
}


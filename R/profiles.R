# Functions for converting between hybrid levels and pressure and height levels
# Based on information at https://github.com/metno/NWPdocs/wiki/Examples#calculating-model-level-height

check_level_colnames <- function(level_defs) {
  if (!all(sort(colnames(level_defs)) == c("a", "b"))) {
    cli::cli_abort(c(
      "Incorrect column names for {.arg level_defs}!",
      "i" = "{.arg level_defs} must have column names \"a\" and \"b\".",
      "x" = "You supplied {.arg level_defs} with column names {colnames(level_defs)}."
    ), call = rlang::caller_env())
  }
}

hybrid_full_to_hybrid_half <- function(level_defs) {

  check_level_colnames(level_defs)
  a_full <- level_defs$a
  b_full <- level_defs$b

  # Initialize the half levels: a_half = 0 and b_half = 1 at the lowest level
  a_half <- rep(0, length(a_full) + 1)
  b_half <- rep(1, length(b_full) + 1)

  # loop from bottom to the top of the atmosphere (a_full and b_full should be
  # ordered top to bottom)

  for (level in length(a_full):1) {
    a_half[level] <- 2 * a_full[level] - a_half[level + 1]
    b_half[level] <- 2 * b_full[level] - b_half[level + 1]
  }

  data.frame(a = a_half, b = b_half)
}

hybrid_half_to_hybrid_full <- function(level_defs) {

  check_level_colnames(level_defs)
  a_half <- level_defs$a
  b_half <- level_defs$b

  # Initialize the full levels
  a_full <- rep(0, length(a_half) - 1)
  b_full <- rep(1, length(b_half) - 1)

  # Loop from bottom to top of atmosphere (a and b assumed to be ordered from
  # top to bottom)
  for (level in 2:length(a_half)) {
    a_full[level - 1] <- 0.5 * (a_half[level - 1] + a_half[level])
    b_full[level - 1] <- 0.5 * (b_half[level - 1] + b_half[level])
  }

  data.frame(a = a_full, b = b_full)
}


################################################################################
# COMPUTE PRESSURE ON HYBRID LEVELS ############################################
################################################################################

#' Title
#'
#' @param level_defs
#' @param psfc
#'
#' @returns
#' @export
#'
#' @examples
hybrid_to_pressure <- function(level_defs, psfc) {
  check_level_colnames(level_defs)
  UseMethod("hybrid_to_pressure", psfc)
}

#' @export
hybrid_to_pressure.numeric <- function(level_defs, psfc) {
  a <- level_defs$a
  b <- level_defs$b
  if (length(psfc) == 1) {
    return(a + b * psfc)
  }
  mapply(function(a, b) a + b * psfc, a, b)
}

#' @export
hybrid_to_pressure.geofield <- function(level_defs, psfc) {
  a <- level_defs$a
  b <- level_defs$b
  geolist(mapply(function(a, b) a + b * psfc, a, b, SIMPLIFY = FALSE))
}

#' @export
hybrid_to_pressure.harp_xs <- function(level_defs, psfc) {
  res <- dplyr::reframe(
    dplyr::group_by(psfc, dplyr::across(c("distance"))),
    pressure = hybrid_to_pressure(level_defs, .data[["value"]]),
    level_number = seq_len(nrow(level_defs))
  )
  res <- tidyr::unnest(res, "pressure")
  structure(res, class = union("harp_xs", class(res)))
}

#' @export
hybrid_to_pressure.harp_xs_list <- function(level_defs, psfc) {
  a <- level_defs$a
  b <- level_defs$b
  structure(
    lapply(psfc, function(x) hybrid_to_pressure(a, b, x)),
    class = union("harp_xs_list", class(psfc))
  )
}

#' Title
#'
#' @param level_defs
#' @param psfc
#'
#' @returns
#' @export
#'
#' @examples
hybrid_full_to_pressure_half <- function(level_defs, psfc) {
  check_level_colnames(level_defs)
  half_levels <- hybrid_full_to_hybrid_half(level_defs)
  hybrid_to_pressure(half_levels)
}

hybrid_half_to_pressure_full <- function(level_defs, psfc) {
  check_level_colnames(level_defs)
  full_levels <- hybrid_full_to_hybrid_half(level_defs)
  hybrid_to_pressure(full_levels)
}
################################################################################
# COMPUTE HEIGHT ON HYBRID LEVELS FROM PRESSURE ON HYBRID LEVELS ###############
################################################################################

#' Title
#'
#' @param pressure
#' @param temp
#' @param spec_hum
#' @param oro
#'
#' @returns
#' @export
#'
#' @examples
pressure_to_height <- function(pressure, temp, spec_hum, oro = NA) {
  UseMethod("pressure_to_height")
}

#' @export
pressure_to_height.numeric <- function(pressure, temp, spec_hum, oro = NA) {

  n_dims <- check_dimensions(
    list(pressure = pressure, temp = temp, spec_hum = spec_hum)
  )

  if (unique(n_dims) != 1) {
    cli::cli_abort(c(
      "Args must all be 1 dimensional, or {.cls xsection} or {.cls geolist}."
    ))
  }

  dim_diff <- c(
    length(pressure) - length(temp), length(pressure) - length(spec_hum)
  )
  if (!all(dim_diff == 1)) {
    cli::cli_abort(c(
      "{.arg pressure} does not appear to be on half levels.",
      "x" = paste(
        "{.arg pressure} has {length(pressure)} levels and",
        "{.arg temp} and {.arg spec_hum} have",
        "{unique(c(length(temp), length(spec_hum)))} levels."
      ),
      "i" = paste(
        "{.arg pressure} should have 1 more level than",
        "{.arg temp} and {.arg spec_hum}."
      )
    ))
  }

  temp_virtual <- virtual_temp(temp, spec_hum)
  num_levels   <- length(temp_virtual)

  # Compute the height on half levels, from the bottom up
  R <- 287.058 # Universal gas constant for air
  g <- 9.80665
  height_half <- rep(0, num_levels + 1)
  for (k in num_levels:2) {
    height_half[k] <- height_half[k + 1] +
      (R * temp_virtual[k] / g) * log(pressure[k + 1] / pressure[k])
  }

  # Add in ground level if it is passed
  if (!is.null(oro)) {
    height_half <- oro + height_half
  }

  # Compute height on full levels
  height_full <- rep(0, num_levels)
  for (k in num_levels:2) {
    height_full[k] <- (height_half[k + 1] + height_half[k]) / 2
  }

  height_full[1] <- height_half[2] + (R * temp_virtual[1] / g) * log(2)

  height_full
}

#' @export
pressure_to_height.harp_xs <- function(pressure, temp, spec_hum, oro = NA) {
  res <- Reduce(
    function(x, y) dplyr::inner_join(x, y, by = "distance"),
    list(
      dplyr::group_nest(pressure, dplyr::across("distance"), .key = "pressure"),
      dplyr::group_nest(temp, dplyr::across("distance"), .key = "temp"),
      dplyr::group_nest(spec_hum, dplyr::across("distance"), .key = "spec_hum")
    )
  )
  if (inherits(oro, "harp_xs")) {
    colnames(oro)[colnames(oro) == "value"] <- "oro"
    res <- dplyr::inner_join(res, oro, by = "distance")
  } else {
    res <- dplyr::mutate(res, oro = NA)
  }
  res <- dplyr::mutate(
    res,
    height = mapply(
      function(x, y, z, o) data.frame(
        height = pressure_to_height(x$pressure, y$value, z$value, o),
        level_number = seq_len(nrow(y))
      ),
      .data[["pressure"]], .data[["temp"]], .data[["spec_hum"]], .data[["oro"]],
      SIMPLIFY = FALSE
    )
  )
  res <- structure(
    tidyr::unnest(
      dplyr::select(res, dplyr::all_of(c("distance", "height"))),
      "height"
    ),
    class = union("harp_xs", class(res))
  )
  res
}

#' @export
pressure_to_height.harp_xs_list <- function(
  pressure, temp, spec_hum, oro = NULL
) {
  stopifnot(inherits(temp, "harp_xs_list"))
  stopifnot(inherits(spec_hum, "harp_xs_list"))
  if (!is.null(oro)) {
    stopifnot(inherits(oro, "harp_xs_list") || inherits(oro, "harp_xs"))
  }

  if (is.null(oro)) {
    return(structure(
      mapply(
        pressure_to_height, pressure, temp, spec_hum, SIMPLIFY = FALSE
      ),
      class = union("harp_xs_list", class(pressure))
    ))
  }
  if (inherits(oro, "harp_xs_list")) {
    return(structure(
      mapply(
        pressure_to_height, pressure, temp, spec_hum, oro,
        SIMPLIFY = FALSE
      ),
      class = union("harp_xs_list", class(pressure))
    ))
  }
  if (inherits(oro, "harp_xs")) {
    return(structure(
      mapply(
        pressure_to_height, pressure, temp, spec_hum, SIMPLIFY = FALSE,
        MoreArgs = list(oro = oro)
      ),
      class = union("harp_xs_list", class(pressure))
    ))
  }
}

################################################################################
# COMPUTE HEIGHT ON HYBRID LEVELS ##############################################
################################################################################

#' Title
#'
#' @param level_defs
#' @param psfc
#' @param temp
#' @param spec_hum
#' @param oro
#'
#' @returns
#' @export
#'
#' @examples
hybrid_to_height <- function(level_defs, psfc, temp, spec_hum, oro = NA){
  check_level_colnames(level_defs)
  pressure <- hybrid_to_pressure(level_defs, psfc)
  pressure_to_height(pressure, temp, spec_hum, oro)
}

check_dimensions <- function(x, diff_len) {
  n_dims <- num_dims(x)
  if (length(unique(n_dims)) != 1) {
    cli::cli_abort(c(
      "{.arg {names(x)}} do not have the same number of dimensions.",
      "x" = "{.arg {names(x)}} have {n_dims} dimensions."
    ), call = rlang::caller_env())
  }
  n_dims
}

get_dim <- function(x) {
  dim_x <- dim(x)
  if (is.null(dim_x)) {
    dim_x <- length(x)
  }
  dim_x
}

num_dims <- function(x) {
  vapply(lapply(x, get_dim), length, numeric(1))
}

y_to_even_y <- function(y, y_vals, new_y, oob) {
  rule <- switch(oob, "censor" = 1, 2)
  res <- approx(y, y_vals, new_y, rule = rule)
  data.frame(
    level = res$x,
    value = res$y
  )
}

model_xs_to_grid_height <- function(data, height, res = 10, oob = c("censor", "squish")) {
  oob <- match.arg(oob)
  height_col <- intersect(colnames(height), c("height", "value"))
  max_y <- ceiling(max(height[[height_col]]) / res) * res
  min_y <- floor(min(height[[height_col]]) / res) * res
  new_y <- seq(max_y, min_y, -res)

  model_xs_to_grid(data, height, height_col, new_y, oob)

}

model_xs_to_grid_pressure <- function(data, pressure, res = 100, oob = c("censor", "squish")) {
  oob <- match.arg(oob)

  # Linear interpolation should be done in log(P)
  press_col <- intersect(colnames(pressure), c("pressure", "value"))

  max_y <- ceiling(max(pressure[[press_col]]) / res) * res
  min_y <- floor(min(pressure[[press_col]]) / res) * res
  new_y <- seq(min_y, max_y, res)

  # Linear interpolation should be done in log(P)
  pressure[[press_col]] <- log(pressure[[press_col]])
  new_y <- log(new_y)

  res <- model_xs_to_grid(data, pressure, press_col, new_y, oob)
  res[[press_col]] <- exp(res[[press_col]])
  res

}

model_xs_to_grid <- function(data, y, y_col, new_y, oob) {
  data <- dplyr::inner_join(
    dplyr::group_nest(data, dplyr::across("distance"), .key = "xs"),
    dplyr::group_nest(y, dplyr::across("distance"), .key = "y"),
    by = "distance"
  )

  data <- dplyr::mutate(data,
    value = mapply(
      function(y, v) y_to_even_y(y[[y_col]], v[["value"]], new_y, oob),
      .data[["y"]], .data[["xs"]],
      SIMPLIFY = FALSE
    )
  )

  data <- tidyr::unnest(
    dplyr::select(data, dplyr::all_of(c("distance", "value"))), "value"
  )
  colnames(data)[colnames(data) == "level"] <- y_col

  structure(data, class = union("harp_xs", class(data)))

}

#' Title
#'
#' @param x
#' @param psfc
#' @param level_defs
#' @param temp
#' @param spec_hum
#' @param topo
#' @param topo_is_geo
#' @param vert_res
#' @param out_of_bounds
#'
#' @returns
#' @export
#'
#' @examples
hybrid_xs_to_grid <- function(
  x,
  psfc,
  level_defs = harmonie_levels$full65,
  temp = NA,
  spec_hum = NA,
  topo = NA,
  topo_is_geo = TRUE,
  vert_res = "auto",
  out_of_bounds = c("censor", "squish")
) {
  out_of_bounds <- match.arg(out_of_bounds)
  UseMethod("hybrid_xs_to_grid")
}

#' @export
hybrid_xs_to_grid.harp_xs <- function(
  x,
  psfc,
  level_defs = harmonie_levels$full65,
  temp = NA,
  spec_hum = NA,
  topo = NA,
  topo_is_geo = TRUE,
  vert_res = "auto",
  out_of_bounds = c("censor", "squish")
) {

  num_levels <- length(x$distance[x$distance == min(x$distance)])

  convert_to <- "pressure"
  vert_unit  <- "Pa"
  if (is_harp_xs(temp) && is_harp_xs(spec_hum)) {
    if (is_harp_xs(topo) && topo_is_geo) {
      topo <- geopotential_to_height(topo)
    }
    convert_to <- "height"
    vert_unit  <- "m"
  }

  vert_res <- get_vert_res(vert_res, convert_to, vert_unit)

  if (convert_to == "pressure") {
    return(model_xs_to_grid_pressure(
      x,
      hybrid_to_pressure(level_defs, psfc),
      vert_res,
      out_of_bounds
    ))
  }

  if (convert_to == "height") {
    if (num_levels == nrow(level_defs)) {
      level_defs <- hybrid_full_to_hybrid_half(level_defs$a, level_defs$b)
    }
    return(model_xs_to_grid_height(
      x,
      hybrid_to_height(level_defs, psfc, temp, spec_hum, topo),
      vert_res,
      out_of_bounds
    ))
  }

}

#' @export
hybrid_xs_to_grid.harp_xs_list <- function(
  x,
  psfc,
  level_defs = harmonie_levels$full65,
  temp = NA,
  spec_hum = NA,
  topo = NA,
  topo_is_geo = TRUE,
  vert_res = "auto",
  out_of_bounds = c("censor", "squish")
) {

  convert_to <- "pressure"
  vert_unit  <- "Pa"
  if (is_harp_xs_list(temp) && is_harp_xs_list(spec_hum)) {
    convert_to <- "height"
    vert_unit  <- "m"
  }


  vert_res <- get_vert_res(vert_res, convert_to, vert_unit)

  if (convert_to == "pressure") {
    res <- structure(
      mapply(
        function(x, psfc) {
          suppressMessages(hybrid_xs_to_grid(
            x, psfc, level_defs, temp, spec_hum, topo, topo_is_geo,
            vert_res, out_of_bounds
          ))
        },
        x, psfc,
        SIMPLIFY = FALSE
      ),
      class = class(x)
    )
  }

  if (convert_to == "height") {
    res <- structure(
      mapply(
        function(x, psfc, temp, spec_hum, topo) {
          suppressMessages(hybrid_xs_to_grid(
            x, psfc, level_defs, temp, spec_hum, topo, topo_is_geo,
            vert_res, out_of_bounds
          ))
        },
        x, psfc, temp, spec_hum, topo,
        SIMPLIFY = FALSE
      ),
      class = class(x)
    )
  }

  message(" ", cli::col_br_green(cli::symbol$tick))
  res

}



is_harp_xs <- function(x) {
  inherits(x, "harp_xs")
}

is_harp_xs_list <- function(x) {
  inherits(x, "harp_xs_list")
}

get_vert_res <- function(vert_res, convert_to, vert_unit) {
  if (!is.numeric(vert_res)) {
    if (vert_res == "auto") {
      vert_res <- switch(convert_to, "pressure" = 100, "height" = 10)
    } else {
      cli::cli_abort(c(
        "{.arg vert_res} not valid.",
        "i" = "{.arg vert_res must be either \"auto\" or numeric.",
        "x" = "You supplied {.arg vert_res} = {vert_res}!"
      ), call = rlang::caller_env())
    }
  }

  message(
    paste(
      "Interpolating from", cli::col_blue("hybrid"), "levels to",
      cli::col_yellow(convert_to), "levels every", paste0(vert_res, vert_unit)
    ),
    appendLF = FALSE
  )

  vert_res
}

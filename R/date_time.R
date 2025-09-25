### Date-time conversion functions.
#
# All functions assume UTC
#

#' Convert to date-time string
#'
#' These functions take a POSIXct or a numeric object and convert to a date-time
#' string with the format specified by the function. Years are always 4 digits
#' long with all other parts of the string 2 digits long with leading zeros
#' where necessary. In the case of numeric input, the number is assumed to be in
#' seconds since 1970-01-01 00:00:00 in UTC.
#'
#' \code{as_str_dttm} returns a string with trailing zeros truncated. The
#' other functions are named such that the output is truncated to the precision
#' given by the function name. Note that no rounding is done, only truncation.
#' There are all lower case versions of functions as well as those with Y, M, D
#' in upper case.
#'
#' @param x A POSIXct or numeric object in seconds since 1970-01-01 00:00:00
#'
#' @return A date-time string
#' @export
#'
#' @examples
#' as_str_dttm(as.POSIXct("2022-03-08 00:00:00"))
#' as_str_dttm(as.POSIXct("2022-03-08 10:00:00"))
#' as_str_dttm(as.POSIXct("2022-03-08 10:30:00"))
#' as_str_dttm(as.POSIXct("2022-03-08 10:30:20"))
#' as_YMD(Sys.time())
#' as_YMDh(Sys.time())
#' as_YMDhm(Sys.time())
#' as_YMDhms(Sys.time())
#' as_YMD(as.numeric(Sys.time()))
#' as_YMDh(as.numeric(Sys.time()))
#' as_YMDhm(as.numeric(Sys.time()))
#' as_YMDhms(as.numeric(Sys.time()))
#' @export
as_str_dttm <- function(x) {
  x <- as_YMDhms(x)

  trailing_0s <- min(nchar(x) - nchar(sub("0*$", "", x)))

  if (!trailing_0s %% 2 == 0) {
    trailing_0s <- trailing_0s - 1
  }

  if (trailing_0s < 1) {
    return(x)
  }

  substr(x, 1, 14 - trailing_0s)
}

#' @rdname as_str_dttm
#' @export
as_YMD <- function(x) {
  UseMethod("as_YMD")
}

#' @export
as_YMD.POSIXct <- function(x) {
  format(x, "%Y%m%d")
}

#' @export
as_YMD.numeric <- function(x) {
  as_YMD(as.POSIXct(x, origin = "1970-01-01 00:00:00", tz = "UTC"))
}

#' @rdname as_str_dttm
#' @export
as_ymd <- function(x) {
  as_YMD(x)
}

#' @rdname as_str_dttm
#' @export
as_YMDh <- function(x) {
  UseMethod("as_YMDh")
}

#' @export
as_YMDh.POSIXct <- function(x) {
  format(x, "%Y%m%d%H")
}

#' @export
as_YMDh.numeric <- function(x) {
  as_YMDh(as.POSIXct(x, origin = "1970-01-01 00:00:00", tz = "UTC"))
}

#' @rdname as_str_dttm
#' @export
as_ymdh <- function(x) {
  as_YMDh(x)
}

#' @rdname as_str_dttm
#' @export
as_YMDhm <- function(x) {
  UseMethod("as_YMDhm")
}

#' @export
as_YMDhm.POSIXct <- function(x) {
  format(x, "%Y%m%d%H%M")
}

#' @export
as_YMDhm.numeric <- function(x) {
  as_YMDhm(as.POSIXct(x, origin = "1970-01-01 00:00:00", tz = "UTC"))
}

#' @rdname as_str_dttm
#' @export
as_ymdhm <- function(x) {
  as_YMDhm(x)
}

#' @rdname as_str_dttm
#' @export
as_YMDhms <- function(x) {
  UseMethod("as_YMDhms")
}

#' @export
as_YMDhms.POSIXct <- function(x) {
  format(x, "%Y%m%d%H%M%S")
}

#' @export
as_YMDhms.numeric <- function(x) {
  as_YMDhms(as.POSIXct(x, origin = "1970-01-01 00:00:00", tz = "UTC"))
}

#' @rdname as_str_dttm
#' @export
as_ymdhms <- function(x) {
  as_YMDhms(x)
}

###

#' Convert to date-time (POSIXct) object
#'
#' Given a string of numbers, a conversion is done to the POSIXct date-time
#' class. The string is assumed to be of the form YYYYMMDD, YYYYMMDDhh,
#' YYYYMMDDhhmm, or YYYYMMDDhhmmss with time zone UTC.
#'
#' @param x A numeric character string or numeric object
#'
#' @return A date-time object of class POSIXct
#' @export
#'
#' @examples
#' # Character input
#' as_dttm("20220203")
#' as_dttm("2022020306")
#' as_dttm("202202030630")
#' as_dttm("20220203063022")
#' #
#' # Numeric input
#' as_dttm(20220203)
#' as_dttm(2022020306)
#' as_dttm(202202030630)
#' as_dttm(20220203063022)
#'
as_dttm <- function(x) {
  UseMethod("as_dttm")
}

#' @export
as_dttm.character <- function(x) {
  unixtime_to_dttm(as_unixtime(x))
}

#' @export
as_dttm.numeric <- function(x) {
  as_dttm(as.character(x))
}




#' Convert to UNIX time - seconds since 1970-01-01 00:00:00
#'
#' Given a string of numbers or a POSIXct date-time object, a conversion is done
#' to UNIX time, i.e. seconds since 1970-01-01 00:00:00. The string is assumed
#' to be of the form YYYYMMDD, YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss with
#' time zone UTC.
#'
#' @param x A numeric character string, numeric or POSIXct object
#'
#' @return Numeric - seconds since 1970-01-01 00:00:00
#' @export
#'
#' @examples
#' # Character input
#' as_unixtime("20220203")
#' as_unixtime("2022020306")
#' as_unixtime("202202030630")
#' as_unixtime("20220203063022")
#' #
#' # Numeric input
#' as_unixtime(20220203)
#' as_unixtime(2022020306)
#' as_unixtime(202202030630)
#' as_unixtime(20220203063022)
#' #
#' # POSIXct input
#' as_unixtime(Sys.time())
as_unixtime <- function(x) {
  UseMethod("as_unixtime")
}

#' @export
as_unixtime.POSIXct <- function(x) {
  as.numeric(x)
}

#' @export
as_unixtime.character <- function(x) {
  scalar_fun <- function(y) {
    if (any(is.na(suppressWarnings(as.numeric(y))))) {
      stop("`x` must be a string of numbers", call. = FALSE)
    }
    if (!nchar(y) %in% c(8, 10, 12, 14)) {
      stop("`x` must be 8, 10, 12 or 14 characters long.", call. = FALSE)
    }
    if (nchar(y) == 8) {
      return(as.POSIXct(y, tz = "UTC", format = "%Y%m%d"))
    }
    if (nchar(y) == 10) {
      return(as.POSIXct(y, tz = "UTC", format = "%Y%m%d%H"))
    }
    if (nchar(y) == 12) {
      return(as.POSIXct(y, tz = "UTC", format = "%Y%m%d%H%M"))
    }
    if (nchar(y) == 14) {
      return(as.POSIXct(y, tz = "UTC", format = "%Y%m%d%H%M%S"))
    }
  }
  sapply(x, scalar_fun, USE.NAMES = FALSE)
}


#' @export
as_unixtime.numeric <- function(x) {
  as_unixtime(as_dttm(x))
}

#' Convert unix time to other formats.
#'
#' The unix time is expected to be in seconds since 1970-01-01 00:00:00 for
#' the UTC time zone.
#'
#' @param x A numeric vector of UNIX time in seconds since 1970-01-01 00:00:00.
#' @return A date-time object or a character date-time string in the desired
#'   format
#'
#' @examples
#' unixtime_to_dttm(Sys.time())
#' unixtime_to_ymd(Sys.time())
#' unixtime_to_ymdh(Sys.time())
#' unixtime_to_ymdhm(Sys.time())
#' unixtime_to_ymdhms(Sys.time())
#' unixtime_to_str_dttm(Sys.time())
#' @export
unixtime_to_dttm <- function(x) {
  as.POSIXct(x, tz = "UTC", origin = "1970-01-01 00:00:00")
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_YMD <- function(x) {
  as_ymd(unixtime_to_dttm(x))
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_YMDh <- function(x) {
  as_ymdh(unixtime_to_dttm(x))
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_YMDhm <- function(x) {
  as_ymdhm(unixtime_to_dttm(x))
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_YMDhms <- function(x) {
  as_ymdhms(unixtime_to_dttm(x))
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_ymd <- function(x) {
  as_ymd(unixtime_to_dttm(x))
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_ymdh <- function(x) {
  as_ymdh(unixtime_to_dttm(x))
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_ymdhm <- function(x) {
  as_ymdhm(unixtime_to_dttm(x))
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_ymdhms <- function(x) {
  as_ymdhms(unixtime_to_dttm(x))
}

#' @rdname unixtime_to_dttm
#' @export
unixtime_to_str_dttm <- function(x) {
  as_str_dttm(unixtime_to_dttm(x))
}

###

#' Generate a sequence of time strings
#'
#' @description Given a start date-time, end date-time and time resolution
#' `seq_dttm()` generates a regular sequence of date-time strings is generated.
#' The start and end date-times must be a string or numeric of the form
#' YYYYMMDD, YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss.
#'
#' The output sequence is a vector of strings. Truncation of the strings is done
#' so that the last zero values are removed.
#'
#' `seq_secs()`, `seq_mins()`, `seq_hours()` and `seq_days()` generate regular
#' sequences of numbers as character vectors with a character specifying the
#' time unit. These vectors can be used to, for example, generate sequences of
#' lead times for input to functions such as
#' \code{\link[harpIO]{read_forecast}}. \code{\link{to_seconds}} can be used to
#' convert any of the
#'
#' @param start_dttm The date-time at the start of the sequence. Must be a
#'   string or numeric of the form YYYYMMDD, YYYYMMDDhh, YYYYMMDDhhmm, or
#'   YYYYMMDDhhmmss.
#' @param end_dttm The date-time at the end of the sequence. Must be a string or
#'   numeric of the form YYYYMMDD, YYYYMMDDhh, YYYYMMDDhhmm, or YYYYMMDDhhmmss.
#' @param by Increment of the sequence. If numeric, it is considered to be in
#'   hours, otherwise a string with a number followed by a unit. Units can be
#'   "s", for seconds; "m", for minutes; "h", for hours; or "d", for days.
#'
#' @return A sequence of date-time strings
#' @export
#'
#' @examples
#' seq_dttm(20220306, 20220307)
#' seq_dttm(20220306, 20220307, by = "30m")
#' seq_dttm(20220301, 20220331, by = "1d")
#' seq_dttm(202203061030, 202203061045, by = "30s")
seq_dttm <- function(start_dttm, end_dttm, by = "1h") {

  if (length(by) != 1) {
    stop("Only one value can be given for `by`.")
  }

  start_dttm <- as_unixtime(start_dttm)
  end_dttm   <- as_unixtime(end_dttm)

  if (is.numeric(by)) {
    by <- paste0(by, "h")
  }

  by_secs <- to_seconds(by)

  if (is.na(by_secs)) {
    stop("Unable to parse units for `by`. Use d, h, m or s. e.g. by = \"6h\"")
  }

  as_str_dttm(seq(start_dttm, end_dttm, by_secs))
}

#' @inheritParams base::seq
#'
#' @rdname seq_dttm
#' @export
#'
#' @examples
#' seq_secs(0, 60, 5)
seq_secs <- function(from, to, by = 1) {
  paste0(seq(from, to, by), "s")
}

#' @rdname seq_dttm
#' @export
#'
#' @examples
#' seq_mins(0, 60, 15)
seq_mins <- function(from, to, by = 1) {
  paste0(seq(from, to, by), "m")
}

#' @rdname seq_dttm
#' @export
#'
#' @examples
#' seq_hours(0, 6)
#' seq_hours(0, 6, 3)
seq_hours <- function(from, to, by = 1) {
  paste0(seq(from, to, by), "h")
}

#' @rdname seq_dttm
#' @export
#'
#' @examples
#' seq_days(0, 7)
#' seq_days(0, 28, 7)
seq_days <- function(from, to, by = 1) {
  paste0(seq(from, to, by), "d")
}

#' Convert a time period to seconds
#'
#' Convert number of minutes, hours, days or weeks into seconds. If \code{x} is
#' numeric, it is assumed to be in hours.
#'
#' @param x A character or numeric vector. The units of the input is specified
#'   by a single character:
#'   \itemize{
#'     \item s: seconds
#'     \item m: minutes
#'     \item h: hours
#'     \item d: days
#'     \item w: weeks
#'   }
#'
#' @return A named vector where the values are the number of seconds and the
#'   names are the equivalent in the units given in the input
#' @seealso [from_seconds()]
#' @export
#'
#' @examples
#' # Numeric inputs are assumed to be in hours
#' to_seconds(c(0, 1, 2))
#'
#' # The same values given in seconds, minutes and explicitly hours
#' to_seconds(c("0s", "3600s", "7200s"))
#' to_seconds(c("0m", "60m", "120m"))
#' to_seconds(c("0h", "1h", "2h"))
#'
#' # Units can be mixed
#' to_seconds(c(paste0(24 * 7, "h"), "7d", "1w"))
to_seconds <- function(x) {
  UseMethod("to_seconds")
}

#' @export
to_seconds.character <- function(x) {
  num  <- try(as.numeric(substr(x, 1, nchar(x) - 1)))
  if (inherits(num, "try-error")) {
    to_seconds_error(x)
  }
  mult <- time_multiplier(substr(x, nchar(x), nchar(x)))
  if (any(is.na(mult))) {
    to_seconds_error(x)
  }
  result <- num * mult
  names(result) <- x
  result
}

#' @export
to_seconds.numeric <- function(x) {
  to_seconds(paste0(x, "h"))
}

time_multiplier <- Vectorize(function(x) {
  switch(
    x,
    "s" = 1,
    "m" = 60,
    "h" = 3600,
    "d" = 86400,
    "w" = 604800,
    NA
  )
})

to_seconds_error <- function(x) {
  valid_units = "`s`, `m`, `h`, `d`, or `w`"
  cli::cli_abort(c(
    "Cannot convert to seconds.",
    "x" = "{.arg x} = {.var {x}} does not have the correct unit specifier.",
    "i" = "{.arg x} should be a numeric value followed by {valid_units}."
  ))

}

#' Convert seconds to another time unit
#'
#' Given a vector of values in seconds, `from_seconds()` will return the
#' vector in the specified time unit. Values will be a character vector with
#' each element appended with the unit, "s" = seconds, "m" = minutes, "h" =
#' hours, "d" = days and "w" = weeks. Alternatively the function can return the
#' values unchanged, but with names added in the specified unit. If the input
#' already has names, the required unit will be inferred from those names.
#'
#' There are also a couple of convenience functions that act as wrappers
#' around `from_seconds()` - `add_seconds_names()` simply adds names in the
#' specified unit to the input, ignoring any names that are already there, and
#' `fix_seconds_names()` ensures that the names are correct with the unit
#' implied from the names that are already there. This is useful if some
#' mathematical operation has been done and the names need updating.
#'
#' @param x A vector of numeric values in seconds.
#' @param to The unit to convert from seconds to. Can be "s", "m", "h", "d", "w"
#'   for seconds, minutes, hours, days and weeks respectively. The default is to
#'   convert to hours.
#' @param as_names Logical. Set to `TRUE` to return `x` unchanged, but with the
#'   values in the new units as names. Default is `FALSE`
#' @param from_names Logical. Set to `TRUE` to get the time unit to convert to
#'   from the names of `x`. In this case `to` is ignored. Default is FALSE.
#' @param num_dp If non integer values are returned, the number of decimal
#'   places to include in the values in the new units. Set to `NULL` (the
#'   default) to format "as is".
#'
#' @returns A character vector or a named numeric vector.
#' @seealso [to_seconds()]
#' @export
#'
#' @examples
#'
#' from_seconds(c(0, 3600, 7200, 10800))
#' from_seconds(c(0, 3600, 7200, 10800), as_names = TRUE)
#'
#' # Get time unit from names
#' tt <- to_seconds(seq_hours(0, 9, 3))
#' tt
#' from_seconds(tt, from_names = TRUE)
#'
#' # Fix the names after mathematical operation
#' tt + 3600 # Wrong names!
#' fix_seconds_names(tt + 3600) # Correct names
from_seconds <- function(
    x, to = "h", as_names = FALSE, from_names = FALSE, num_dp = NULL
) {

  unit <- NULL
  if (from_names) {
    unit <- regmatches(names(x), regexpr("[a-z]$|[A-Z]$", names(x)))
  } else {
    unit <- to
  }

  scl_fctr <- time_multiplier(tolower(unit))
  if (any(is.na(scl_fctr))) {
    allowed_units <- paste0(
      cli::col_br_blue('"'),
      glue::glue_collapse(
        cli::col_br_blue(c("s", "m", "h", "d", "w")),
        sep = paste0(cli::col_br_blue('"'), ", "),
        last = paste(cli::col_br_blue('"'), "and/or", cli::col_br_blue('"'))
      ),
        cli::col_br_blue('"')
    )
    cli::cli_abort(c(
      "Cannot convert seconds to specified unit.",
      "x" = "You asked for seconds in {.val {unique(unit)}}.",
      "i" = "{.arg to} or {.arg names(x)} must be in units of {allowed_units}."
    ))
  }

  scaled <- x / scl_fctr
  if (any((scaled / floor(scaled)) != floor(scaled)) && !is.null(num_dp)) {
    scaled <- trimws(format(round(scaled, num_dp), nsmall = num_dp))
  }
  if (as_names) {
    names(x) <- paste0(scaled, unit)
  } else {
    x <- paste0(scaled, unit)
  }
  x
}

#' @rdname from_seconds
#' @export
fix_seconds_names <- function(x) {
  from_seconds(x, as_names = TRUE, from_names = TRUE)
}

#' @rdname from_seconds
#' @export
add_seconds_names <- function(x, to = "h") {
  from_seconds(unname(x), to = to, as_names = TRUE)
}

#' Round date-times to defined multiples
#'
#' `round_dttm()` takes a vector of POSIXct date-times and rounds them to
#' prescribed multiples. Date-times can be rounded to the nearest multiple, or
#' up or down. Additionally, an offset can be applied.
#'
#' @param dttm A vector of POSIXct date-times.
#' @param rounding The multiple to which to round date-times. This
#'   should be a number followed "s", "m", "h", or "d" for seconds, minutes,
#'   hours and days.
#' @param direction The direction in which to round date-times. Can be
#'   "nearest", "up" or "down". For "nearest", the rounding is centred on
#'   `rounding`. Where there is an even number of date-times to be
#'   aggregated, the averaging window favours date-times before the rounding
#'   time. For example, if rounding date-times to "6h", the aggregation for 06
#'   UTC will take in 03, 04, 05, 06, 07 and 08 UTC, and the aggregation for 12
#'   UTC will take in 09, 10, 11, 12, 13 and 14 UTC etc. For "up" the rounding
#'   is for a window for all times up to and including `rounding` and for
#'   "down" the rounding is for a window starting at `rounding`.
#' @param offset The offset to be applied to `rounding`. This
#'   should be a number followed "s", "m", "h", or "d" for seconds, minutes,
#'   hours and days. This is used to centre the rounding. For example, if
#'   `rounding` = "1d", the rounding will be centred on 00 UTC. If you want
#'   it to be centred on 12 UTC, you would supply an offset of "12h". Note that
#'   the offset is applied backwards in time, so if you want to centre on 06
#'   UTC, for example, the offset should be "18h".
#'
#' @return A POSIXct vector of the same length as `dttm` with rounded
#'   date-times.
#' @export
#'
#' @examples
#' my_dttm <- data.frame(dttm = as_dttm(seq_dttm(2024010100, 2024010223)))
#'
#' # Round to the nearest 6h - note that 06 UTC includes 03, 04, 05, 06, 07 and
#' # 08 UTC
#' my_dttm$rounded <- round_dttm(my_dttm$dttm, "6h")
#' my_dttm
#'
#' # Round up to the nearest 6h
#' my_dttm$rounded <- round_dttm(my_dttm$dttm, "6h", "up")
#' my_dttm
#'
#' # Round to the nearest day - note centring on 00 UTC
#' my_dttm$rounded <- round_dttm(my_dttm$dttm, "1d")
#' my_dttm
#'
#' # Centre 1 day rounding on 12 UTC
#' my_dttm$rounded <- round_dttm(my_dttm$dttm, "1d", offset = "12h")
#' my_dttm
round_dttm <- function(dttm, rounding, direction= "nearest", offset = 0) {
  if (!inherits(dttm, "POSIXct")) {
    cli::cli_abort(c(
      "Invalid class for {.arg dttm}.",
      "x" = "You supplied an object of class {.cls {class(dttm)}}.",
      "i" = "{.arg dttm} must have class {.cls POSIXct}."
    ))
  }
  dttm       <- as.numeric(dttm) + to_seconds(offset)
  rounding   <- harpCore::to_seconds(rounding)
  round_func <- get(paste0("round_", direction))
  harpCore::unixtime_to_dttm(
    round_func(dttm, rounding) - to_seconds(offset)
  )
}

round_nearest <- function(x, mult) {
  floor((x + mult / 2) / mult) * mult
}

round_up <- function(x, mult) {
  x + (-x %% mult)
}

round_down <- function(x, mult) {
  x - (x %% mult)
}


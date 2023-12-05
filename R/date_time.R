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
#'     \item{s} {seconds}
#'     \item{m} {minutes}
#'     \item{h} {hours}
#'     \item{d} {days}
#'     \item{w} {weeks}
#'   }
#'
#' @return A named vector where the values are the number of seconds and the
#'   names are the equivalent in the units given in the input
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

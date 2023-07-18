# useful functions that have no special purpose related to harp

#' Generate a sequence of integer powers of 2
#'
#' A sequence of powers of 2 is generated based on the first value and the
#' length of the desired sequence. The first value could either be the first
#' power in the sequence, or it could be the first value of the sequence. The
#' `powers` argument is used to tell the function what the first value is.
#'
#' The function only generates sequences of 2 raised to integer powers. It can
#' be useful in generating breaks values for exponential colour scales.
#'
#' @param from The starting power or the value of the first number in the
#'   sequence. In the case of the latter, this must be value that is 2
#'   raised to an integer power.
#' @param len The length of the desired sequence.
#' @param powers Logical. Whether the first argument is the first power to
#'   begin the sequence from, or whether it is the value to begin the sequence
#'   from.
#'
#' @return A numeric vector of consecutive powers of 2.
#' @export
#'
#' @examples
#' # Start from 1
#' seq_pwr2(0, 5)
#' seq_pwr2(1, 5, powers = FALSE)
#' # Start from a value larger than 1
#' seq_pwr2(3, 5)
#' seq_pwr2(8, 5, powers = FALSE)
#' # Start from a value smaller than 1
#' seq_pwr2(-3, 5)
#' seq_pwr2(0.125, 5, powers = FALSE)
seq_pwr2 <- function(from, len, powers = TRUE) {
  if (!is.logical(powers)) {
    stop("`powers` must be logical: TRUE or FALSE")
  }
  if (powers) {
    if (round(from) != from) {
      stop("`from` must be an integer when powers = TRUE")
    }
  } else {
    if (from < 0 || is.infinite(log2(from)) || round(log2(from)) != log2(from)) {
      stop("`from` must be an integer power of 2 when powers = FALSE")
    }
  }
  if (len < 1 || round(len) != len) {
    stop("`len` must be a positive integer")
  }

  if (powers) {
    return(2 ^ seq(from, length.out = len))
  }
  2 ^ seq(log2(from), length.out = len)
}

#' Generate a doubled sequence
#'
#' \code{seq_double} generates a sequence of numbers where each value is double
#' the previous value in the sequence.
#'
#' @param start The first number in the sequence.
#' @param len The length of the sequence.
#'
#' @return A numeric vector
#' @export
#'
#' @examples
#' seq_double(1, 5)
#' seq_double(0.125, 10)
seq_double <- function(start, len) {
  cumprod(c(start, rep(2, (len - 1))))
}


#' Piecewise pattern matching and replacement
#'
#' \code{psub} calls \code{\link[base]{gsub}} for each pattern / replacement
#' pair. Where a pattern is not found, a warning is issued and nothing is
#' replaced for that pattern / replacement pair. Unlike
#' \code{\link[base]{gsub}}, the first argument is the vector where matches
#' are sought, meaning that it can be easily used in a pipeline.
#'
#' The default behaviour is to look for an exact match of pattern. This means
#' that only those elements in \code{x} that fully match \code{pattern} are
#' replaced. See examples for how this works in practice.
#'
#' @param x A character vector.
#' @param pattern A vector of patterns to replace.
#' @param replacement A character vector of the same length as \code{pattern}
#'   with the corresponding replacements.
#' @param exact A logical to denote whether \code{pattern} is to be an exact
#'   match (the default). When FALSE, \code{pattern} is treated as a regular
#'   expression.
#' @param ... Other arguments to \code{gsub}.
#'
#' @return A character vector of the same length as \code{x} with matched
#'   patterns replaced.
#' @export
#'
#' @examples
#' # Capitalise some letters
#' psub(letters[1:7], c("a", "c", "e"), c("A", "C", "E"))
#'
#' # By default exact matches are sought
#' psub("ace", c("a", "c", "e"), c("A", "C", "E"))
#'
#' # Use exact = FALSE to replace a regular expression
#' psub("ace", c("a", "c", "e"), c("A", "C", "E"), exact = FALSE)
#'
#' # Replace selected values in a vector
#' psub(c("one", "two", "three"), c("one", "three"), c("ONE", "THREE"))
psub <- function(x, pattern, replacement, exact = TRUE, ...) {

  if (!is.character(x)) {
    stop("`x` must be of type character")
  }
  if (!is.character(pattern)) {
    stop("`pattern` must be of type character")
  }
  if (!is.character(replacement)) {
    stop("`replacement` must be of type character")
  }

  if (length(pattern) != length(replacement)) {
    stop("`pattern` and `replacement` must be the same length")
  }

  for (i in seq_along(pattern)) {
    gsub_pattern <- pattern[i]
    if (exact) {
      if (substr(gsub_pattern, 1, 1) != "^") {
        gsub_pattern <- paste0("^", gsub_pattern)
      }
      if (substr(gsub_pattern, nchar(gsub_pattern), nchar(gsub_pattern)) != "$") {
        gsub_pattern <- paste0(gsub_pattern, "$")
      }
    }
    if (!any(grepl(gsub_pattern, x, ...))) {
      warning("\"", pattern[i], "\" not found in `x`.")
    }
    x <- gsub(gsub_pattern, replacement[i], x, ...)
  }

  x
}

#' Extract numeric values from a string or vector of strings
#'
#' If you have a string that contains characters and numbers,
#' \code{extract_numeric} will extract the numeric parts. Anything between the
#' numerics in the string will result in a separate element in the output.
#'
#' If a vector of strings is passed and you want to return something of the
#' same length, set \code{delist = FALSE} and a list will be returned with the
#' same length as the input vector.
#'
#' @param x A character vector.
#' @param as_list Logical. Whether to return a list. See Details.
#'
#' @return A vector or list of numerics
#' @export
#'
#' @examples
#' extract_numeric("3h")
#'
#' # With decimals
#' extract_numeric("3h 4h 5.5h")
#'
#' # With negative values
#' extract_numeric("-2s4s 6s")
#'
#' # With a vector of values
#' extract_numeric(c("2h", "3h", "7h"))
#' extract_numeric(c("2h", "3h 4h 5h 6h", "7h"))
#'
#' # Return a list of the same length as the vector
#' extract_numeric(c("2h", "3h 4h 5h 6h", "7h"), as_list = TRUE)
extract_numeric <- function(x, as_list = FALSE) {
  if (is.numeric(x)) {
    return(x)
  }
  stopifnot(is.character(x))
  x <- regmatches(
    x,
    gregexpr(
      "(?>-)*[0-9()]+\\.*[0-9()]*",
      x,
      perl = TRUE
    )
  )
  if (as_list) {
    return(lapply(x, as.numeric))
  }
  as.numeric(unlist(x))
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @usage lhs \%>\% rhs
NULL

#' Decide whether thresholds are absolute or quantiles
#'
#' For quantiles, thresholds should be expressed as a character vector with
#' each element beginning with "q" followed by the quantile expressed as a
#' value between 0 and 1.
#'
#' @param x A numeric or character vector.
#'
#' @return A list with a numeric vector of thresholds and a logical indicating
#'   whether the thresholds are quantiles.
#' @export
#'
#' @examples
#' parse_thresholds(seq(1, 5))
#' parse_thresholds(paste0("q", seq(0, 1, 0.2)))
parse_thresholds <- function(x) {
  if (is.numeric(x)) {
    return(list(thresholds = x, quantiles = FALSE))
  }
  if (!is.character(x)) {
    stop(
      "x must be a numeric vector or a chracter vector with elements ",
      "beginning with 'q'."
    )
  }
  if (all(!is.na(suppressWarnings(as.numeric(x))))) {
    return(list(thresholds = x, quantiles = FALSE))
  }
  x <- suppressWarnings(as.numeric(sub("^q", "", x)))
  if (any(is.na(x))) {
    stop(
      "x must be a numeric vector or a chracter vector with elements ",
      "beginning with 'q'."
    )
  }
  if (min(x) < 0 || max(x) > 1) {
    stop(
      "For quantile thresholds, the numeric part must be between 0 and 1."
    )
  }
  list(thresholds = x, quantiles = TRUE)
}

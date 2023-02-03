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

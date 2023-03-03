#' Convert a list of geofields to a geolist
#'
#' \code{geolists} are an integral data structure for harp. They allow for many
#' 2d fields to be stored in columns in data frames so that other dimensions are
#' defined by other columns in the data frame. This avoids the need for
#' complicated dimensions for multidimensional data. A number of methods are
#' implemented for \code{geolists} enabling many elementwise mathematical
#' operations to be done efficiently.
#'
#' All geofields in a geolist must be on the same domain, but they can have
#' different parameters, vertical levels, times etc.
#'
#' @param ... list of geofields, or comma separated geofields.
#'
#' @return A geolist
#' @export
#'
#' @examples
#' # Examples to come with the addition of test data
as_geolist <- function(...) {

  x <- list(...)

  if (length(x) == 1 && is.list(x[[1]])) {
    x <- x[[1]]
  }

  if (length(x) < 1) return(NULL)

  if (inherits(x, "geolist")) {
    return(x)
  }

  # NULLS are allowed in geolists?
  if (!all(sapply(x[!sapply(x, is.null)], meteogrid::is.geofield))) {
    stop("All ... must be geofields.")
  }

  domain_check <- check_geolist_domains(x)
  if (!domain_check) {
    stop("All geofields must be on the same domain.")
  }

  structure(
    x,
    class = c("geolist", class(x)),
    domain = get_geodomain(x)
  )
}

get_geodomain <- function(x) {
  x <- x[!sapply(x, is.null)]
  if (length(x) < 1) {
    return(NULL)
  }
  meteogrid::as.geodomain(x[[1]])
}
check_domains <- function(x, y) {
  if (
    (meteogrid::is.geodomain(x) || meteogrid::is.geofield(x)) &&
    (meteogrid::is.geodomain(y) || meteogrid::is.geofield(y))
  ) {
    return(
      meteogrid::compare.geodomain(
        meteogrid::as.geodomain(x),
        meteogrid::as.geodomain(y)
      )
    )
  }
  if ((length(x) == 0 && is.null(x)) || (length(y) == 0 && is.null(y))) {
    return(NULL)
  }
  FALSE
}

check_geolist_domains <- function(x) {
  x <- x[!sapply(x, is.null)]
  if (length(x) > 1) {
    same_domains <- sapply(
      seq.int(2, length(x)),
      function(d) check_domains(x[[(d - 1)]], x[[d]])
    )
    return(all(same_domains))
  }
  TRUE
}

geofield_info <- function(x) {
  if (is.null(x)) {
    cat("<NULL>\n")
  } else {
    dom  <- meteogrid::DomainExtent(x)
    info <- attr(x, "info")
    cat(
      paste0("<geofield [", dom[["nx"]], " x ", dom[["ny"]], "]"),
      paste0(info[["name"]], ">"),
      "\n"
    )
  }
}

#' @rdname as_geolist
#' @param x object to be tested
#' @export
is_geolist <- function(x) {
  inherits(x, "geolist") && all(sapply(x, meteogrid::is.geofield))
}

#' @export
print.geolist <- function(x, ...) {
  cat("geolist with", length(x[!sapply(x, is.null)]), "valid geofields:\n\n")
  lapply(x, geofield_info)
  cat("\n")
  print(meteogrid::as.geodomain(x[!sapply(x, is.null)][[1]]))
}

#' @export
`[.geolist` <- function(x, i) {
  as_geolist(NextMethod())
}

#' @export
c.geolist <- function(...) {
  as_geolist(NextMethod())
}

###
# Math generic functions - to the math operation elementwise on each geofield
# in the geolist - the return is the same length as the input

#' @export
Math.geolist <- function(x, ...) {
  # For cumulative functions, use Reduce with accumulate = TRUE, otherwise
  # lapply
  if (.Generic == "cumsum") {
    return(
      as_geolist(Reduce(`+`, x, accumulate = TRUE))
    )
  }

  if (.Generic == "cumprod") {
    return(
      as_geolist(Reduce(`*`, x, accumulate = TRUE))
    )
  }

  if (.Generic == "cummax") {
    return(
      as_geolist(Reduce(pmax, x, accumulate = TRUE))
    )
  }

  if (.Generic == "cummin") {
    return(
      as_geolist(Reduce(pmin, x, accumulate = TRUE))
    )
  }

  return(
    as_geolist(lapply(x, .Generic, ...))
  )

}

### Ops generic functions
# Need to set up for geofields as well, since the generics that return logicals
# do not retain the attributes. In this case only the domain information should
# be kept.

#' @export
Ops.geofield <- function(e1, e2) {
  if (missing(e2)) {
    e2 <- NULL
  }
  # If e1 and e2 are geofields - check they are both on the same domain
  if (
    meteogrid::is.geofield(e2) &&
      meteogrid::is.geofield(e1) &&
      !check_domains(
        meteogrid::as.geodomain(e1),
        meteogrid::as.geodomain(e2)
      )
  ) {
    stop("geofields must be on the same domain.")
  }
  # Can only mix a geofield and a scalar
  if (!is.null(e2) && !all(meteogrid::is.geofield(e1), meteogrid::is.geofield(e2))) {
    if (!meteogrid::is.geofield(e1)) scalar_var <- "e1"
    if (!meteogrid::is.geofield(e2)) scalar_var <- "e2"
    if (length(get(scalar_var)) != 1) {
      stop("geofields can only be combined with other geofields or scalars")
    }
  }

  if (meteogrid::is.geofield(e1)) {
    dom <- meteogrid::as.geodomain(e1)
  } else {
    dom <- meteogrid::as.geodomain(e2)
  }
  if (.Generic == "!") {
    return(meteogrid::as.geofield(!unclass(e1), domain = dom))
  }
  .f  <- get(.Generic, envir = parent.frame(), mode = "function")
  res <- .f(unclass(e1), unclass(e2))
  if (is.null(attr(res, "domain"))) {
    return(meteogrid::as.geofield(res, domain = dom))
  }
  structure(res, class = "geofield")
}

#' @export
Ops.geolist <- function(e1, e2) {
  if (.Generic == "!") {
    return(
      as_geolist(lapply(e1, `!`))
    )
  }
  if (is_geolist(e2) && is_geolist(e1)) {
    if (length(e2) != length(e1)) {
      stop("geolists must have the same length")
    }
    if (!(check_domains(attr(e1, "domain"), attr(e2, "domain")))) {
      stop("geolists must have the same domain")
    }
    return(
      as_geolist(mapply(.Generic, e1, e2, SIMPLIFY = FALSE))
    )
  }
  if (!is_geolist(e2)) {
    if (length(e2) != 1 && length(e2) != length(e1)) {
      stop(
        "If second value is not a geolist it must be length 1 ",
        "or the same length as the first value"
      )
    }
    if (length(e2) == 1) {
      return(as_geolist(lapply(e1, .Generic, e2)))
    }
  }
  if (!is_geolist(e1)) {
    if (length(e1) != 1 && length(e1) != length(e2)) {
      stop(
        "If first value is not a geolist it must be length 1 ",
        "or the same length as the second value"
      )
    }
    if (length(e1) == 1) {
      return(as_geolist(lapply(e2, .Generic, e1)))
    }
  }
  as_geolist(mapply(.Generic, e1, e2, SIMPLIFY = FALSE))
}

### Summary generic functions

#' @export
Summary.geolist <- function(x, na.rm = FALSE, ...) {

  if (.Generic %in% c("sum", "prod")) {
    fun <- function(x, y, na.rm) {
      if (na.rm) {
        if (.Generic == "sum") {
          x[is.na(x)] <- 0
          y[is.na(y)] <- 0
        } else {
          x[is.na(x)] <- 1
          y[is.na(y)] <- 1
        }
      }
      op  <- switch(.Generic, "sum" = `+`, `*`)
      op(x, y)
    }
  }

  if (.Generic %in% c("min", "max")) {
    fun <- switch(.Generic, min = pmin, pmax)
  }

  if (.Generic %in% c("all", "any")) {
    fun <- function(x, y, na.rm) {
      op <- switch(.Generic, "all" = `&`, `|`)
      res <- op(x, y)
      if (na.rm) {
        NAs <- which(is.na(res), arr.ind = TRUE)
        if (nrow(NAs) > 0) {
          res[NAs] <- pmax(x[NAs], y[NAs], na.rm = TRUE)
        }
        attrs <- attributes(res)
        res <- as.logical(res)
        attributes(res) <- attrs
      }
      res
    }
  }

  if (.Generic == "range") {
    stop("range method not implemented for geolists. Use min and max.")
  }

  reduce_fun <- function(x, y) {
    fun(x, y, na.rm = na.rm)
  }
  Reduce(reduce_fun, x)
}

### Check for NAs elementwise

#' @export
is.na.geofield <- function(x) {
  dom <- meteogrid::as.geodomain(x)
  meteogrid::as.geofield(is.na(unclass(x)), domain = dom)
}

#' @export
is.na.geolist <- function(x) {
  as_geolist(lapply(x, is.na))
}

### Stats methods

#' @export
mean.geolist <- function(x, na.rm = FALSE, ...) {
  if (na.rm) {
    return(sum(x, na.rm = TRUE) / sum(!is.na(x)))
  }
  sum(x, na.rm = FALSE) / length(x)
}

# Base R var and sd are not methods so make new functions

#' Variance and standard deviation
#'
#' \code{\link[stats]{sd}} and \code{\link[stats]{var}} are not implemented
#'   as methods, which means thet cannot be dispatched for geolists. \code{variance}
#'   and \code{std_dev} are implemented as methods and can be used to compute
#'   the standard deviation and variance of goelists in a pointwise manner.
#'   If the input is not a geolist the default functions \code{\link[stats]{sd}} and
#'   \code{\link[stats]{var}} are used.
#' @param x a geolist, numeric vector, matrix or data frame.
#'
#' @param na.rm logical. Should missing values be removed?
#' @param ... Other arguments to \link[stats]{sd} or \link[stats]{var} for non geolists.
#'
#' @return If x is a geolist, a geofield.
#'
#' @export
variance <- function(x, na.rm = FALSE, ...) {
  UseMethod("variance")
}

#' @export
variance.default <- function(x, na.rm = FALSE, ...) {
  stats::var(x, na.rm = na.rm, ...)
}

#' @export
variance.geolist <- function(x, na.rm = FALSE, ...) {
  x_bar <- mean(x, na.rm = na.rm)

  x <- as_geolist(
    lapply(x, function(y) (y - x_bar) ^ 2)
  )

  if (na.rm) {
    return(sum(x, na.rm = TRUE) / (sum(!is.na(x)) - 1))
  }
  sum(x, na.rm = FALSE) / (length(x) - 1)
}

#' @rdname variance
#' @export
std_dev <- function(x, na.rm = FALSE, ...) {
  UseMethod("std_dev")
}

#' @export
std_dev.default <- function(x, na.rm = FALSE, ...) {
  stats::sd(x, na.rm = na.rm)
}

#' @export
std_dev.geolist <- function(x, na.rm = FALSE, ...) {
  sqrt(variance(x, na.rm = na.rm))
}

#' @export
diff.geolist <- function(x, lag = 1, trim = TRUE, ...) {
  if (round(lag) != lag) {
    stop("lag must be an integer.")
  }
  if (lag == 0) {
    return(x)
  }
  if (lag > (length(x) - 1)) {
    stop("lag is too long.")
  }
  if (lag > 0) {
    res <- as_geolist(x - dplyr::lag(unclass(x), n = lag))
    if (trim) {
      return(res[(lag + 1):length(res)])
    } else {
      return(res)
    }
  }
  res <- as_geolist(x - dplyr::lead(unclass(x), n = (lag * -1)))
  if (trim) {
    return(res[1:(length(res) + lag)])
  }
  res
}

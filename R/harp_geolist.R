# vctrs harp_geolist class
#
# Building the harp_geolist class using vctrs - the
# geolist class is a vctrs list_of object. This means
# that many operations for data frames that include a
# harp_geolist column work smoothly, including binding,
# grouped mutates and grouped summaries. Additionally,
# most mathematical and arithmetic operations can be
# handled using the vec_math and vec_arith generics.

#' @importFrom vctrs new_list_of
#' @importFrom vctrs stop_incompatible_op
#' @importFrom vctrs vec_arith_base
#' @importFrom vctrs vec_math_base
#' @importFrom rlang abort
#' @importFrom rlang warn
#' @importFrom meteogrid is.geofield
#' @importFrom meteogrid is.geodomain
#' @importFrom meteogrid as.geofield
#' @importFrom meteogrid compare.geodomain

######################
### Helper functions #
######################

get_domain <- function(x) attr(x, "domain")

check_same_domain <- function(x) {
  if (length(x) < 2) {
    return(TRUE)
  }
  all(
    sapply(
      2:length(x),
      function(i) compare.geodomain(
        as.geodomain(x[[i]]), as.geodomain(x[[(i - 1)]])
      )
    )
  )
}

# lapply function for geolists - operates on a geolist and
# returns a geolist
glapply <- function(X, FUN, ...) {
  if (!is_geolist(X)) {
    abort("`glapply` can only be used on geolists.")
  }
  .f <- function(..., domain) {
    dots <- rlang::list2(...)
    new_geofield(do.call(FUN, dots), domain = domain)
  }
  geolist(lapply(X, .f, domain = get_domain(X), ...))
}

glapply2 <- function(X, Y, FUN, ...) {
  if (!(is_geolist(X) && is_geolist(Y))) {
    abort("`glapply2` can only be used on geolists.")
  }
  if (!compare.geodomain(get_domain(X), get_domain(Y))) {
    abort("`X` and `Y` must be geolists with the same domain.")
  }
  .f <- function(..., domain) {
    dots <- rlang::list2(...)
    new_geofield(do.call(FUN, dots), domain = domain)
  }
  geolist(
    mapply(
      .f, X, Y,
      MoreArgs = list(domain = get_domain(X), ...),
      SIMPLIFY = FALSE
    )
  )
}

###########################################
### Set up geofield prototype and methods #
###########################################

# Elements of a harp_geolists are geofields. In order
# for a harp_geolist prototype to be defined, a geofield
# prototype must exist.

geofield_error <- "`x` must be a 2-d numeric array."

new_geofield <- function(
    x = array(numeric(), c(0, 0)),
  domain = NULL
) {

  if (is.geofield(x)) {
    return(x)
  }

  if (!is.null(domain)) {
    if (!is.geodomain(domain)) {
      abort("`domain` must be a geodomain.")
    }
    if (length(x) != domain[["nx"]] * domain[["ny"]]) {
      abort("`domain` dimensions do not match dimensions of `x`")
    }
    x <- as.geofield(
      array(x, c(domain[["nx"]], domain[["ny"]])),
      domain = domain
    )
  }

  if (length(x) == 0) {
    x <- array(numeric(), c(0, 0))
  }

  if (!is.numeric(x) && !is.logical(x)) {
    abort(geofield_error)
  }

  if (length(dim(x)) != 2) {
    abort(geofield_error)
  }

  if (is.null(domain)) {
    domain <- get_domain(x)
  }

  if (is.null(domain)) {
    return(structure(x, domain = domain, class = "geofield"))
  }
  as.geofield(x, domain = domain)
}

# geofield generic and methods for instantiating a geofield
geofield <- function(x, ...) {
  UseMethod("geofield")
}

geofield.harp_geolist <- function(x, i = 1, ...) {
  new_geofield(x[[i]])
}

geofield.array <- function(x, domain, ...) {
  if (missing(domain) || !is.geodomain(domain)) {
    abort("`domain` must be passed as a geodomain.")
  }
  new_geofield(x, domain)
}

##############################################################
### Set up prototype, validator, instantiator and formatting #
### for harp_geolist                                         #
##############################################################

# Prototype
new_geolist <- function(x = list(), domain = NULL) {
  if (is.null(domain)) {
    # Find the first element of x with a domain attribute
    i <- 1
    while (i <= length(x)) {
      domain <- get_domain(x[[i]])
      if (is.null(domain)) {
        i <- i + 1
      } else {
        i <- length(x) + 1
      }
    }
  }
  if (is.null(domain)) {
    domain <- NA
  }
  new_list_of(
    x, new_geofield(), domain = domain, class = "harp_geolist"
  )
}

# Validator
validate_geolist <- function(x) {
  if (length(x) < 1) {
    return(x)
  }
  valid_indices <- which(
    sapply(x, function(.x) length(.x) > 0)
  )
  if (!all(sapply(x[valid_indices], is.geofield))) {
    abort("All elements of a geolist must be geofields.")
  }
  if (!check_same_domain(x[valid_indices])) {
    abort("All geofields in a geolist must be on the same domain.")
  }
  if (!length(unique(sapply(x[valid_indices], mode))) == 1) {
    abort("All geofields must be the same type.")
  }
  if (
    !is.null(get_domain(x)) && !identical(get_domain(x), get_domain(x[[1]]))
  ) {
    abort("Domain mismatch between geolist and geofields.")
  }
  x
}

# User facing instantiator
geolist <- function(..., domain = NULL) {
  x <- list(...)
  if (length(x) == 1 && is.list(x[[1]])) {
    x <- x[[1]]
  }
  validate_geolist(new_geolist(x, domain = domain))
}

# Check if is geolist
is_geolist <- function(x) {
  inherits(x, "harp_geolist")
}

# Formatting for geolists
format.harp_geolist <- function(x, ...) {
  format_one <- function(i, x, ws) {
    ws <- ws - nchar(i)
    index_txt <- paste0(paste(rep(" ", ws), collapse = ""), "[[", i, "]]")
    if (length(x) == 0) {
      txt <- paste(index_txt, "<empty>\n")
    } else if (!inherits(x, "geofield")) {
      txt <- paste(index_txt, "<not_a_geofield>\n")
    } else {
      dom <- get_domain(x)
      num_na <- sum(is.na(x))
      if (num_na < 1) {
        na_txt <- ""
      } else {
        na_txt <- paste(" NAs:", num_na)
      }
      min_x  <- min(x, na.rm = TRUE)
      max_x  <- max(x, na.rm = TRUE)
      mean_x <- mean(x, na.rm = TRUE)
      min_txt  <- pillar::style_subtle_num(sprintf("%.3f", min_x), min_x < 0)
      max_txt  <- pillar::style_subtle_num(sprintf("%.3f", max_x), max_x < 0)
      mean_txt <- pillar::style_subtle_num(sprintf("%.3f", mean_x), mean_x < 0)
      txt <- paste0(
        index_txt,
        " <geofield [",
        dom$nx,
        " x ",
        dom$ny, "] ",
        "Min = ", min_txt,
        " Max = ", max_txt,
        " Mean = ", mean_txt,
        na_txt,
        ">\n"
      )
    }
    cat(txt)
  }
  max_txt_length <- nchar(length(x))
  invisible(
    mapply(format_one, seq_along(x), x, MoreArgs = list(ws = max_txt_length))
  )
}

obj_print_data.harp_geolist <- function(x, n = 10, ...) {
  if (length(x) != 0) {
    if (length(x) <= n) {
      n <- length(x)
    }
    format(x[1:n])
  }
}

obj_print_footer.harp_geolist <- function(x, n = 10, ...) {
  if (length(x) <= n) {
    return()
  }
  xtra <- length(x) - n
  cat(pillar::style_subtle(paste("#", xtra, "more geofields\n")))
  cat(pillar::style_subtle("# Use `print(n = ...)` to see more\n"))
}

obj_print_header.harp_geolist <- function(x, ...) {
  cat(pillar::style_subtle(paste0("<harp_geolist[", length(x), "]>\n")))
}

# Formatting for geolists in data frame column
pillar_shaft.harp_geolist <- function(x, ...) {
  format_one <- function(x) {
    if (length(x) == 0) {
      x <- "<empty>"
    } else if (!inherits(x, "geofield")) {
      x <- "<not_a_geofield>"
    } else {
      dom <- get_domain(x)
      x <- paste0("<gfld [", dom$nx, " x ", dom$ny, "]>")
    }
    pillar::style_subtle(x)
  }
  pillar::new_pillar_shaft_simple(
    vapply(x, format_one, character(1))
  )
}

# Type abbreviation for data frame headings
vec_ptype_abbr.harp_geolist <- function(x, ...) "hrp_glst"

##########################
### Casting and coercion #
##########################

#Coercion
vec_ptype2.harp_geolist.harp_geolist <- function(x, y, ...) {
  geolist(domain = get_domain(x))
}

vec_ptype2.harp_geolist.geofield <- function(x, y, ...) {
  geolist(domain = get_domain(x))
}

vec_ptype2.geofield.harp_geolist <- function(x, y, ...) {
  geolist(domain = get_domain(x))
}

# Casting
vec_cast.geofield.geofield <- function(x, to, ...) x

vec_cast.harp_geolist.harp_geolist <- function(x, to, ...) x

vec_cast.geofield.harp_geolist <- function(x, to, ...) geolist(x)

#############################################
# geofield Ops methods with double dispatch #
#############################################

# Ensure mathematical operations with geofields return geofields
# This should possibly be in meteogrid?

Ops.geofield <- function(e1, e2) {
  if (missing(e2)) {
    e2 <- structure(list(), class = "MISSING")
  }
  Ops_gfld(.Generic, e1, e2)
}

Ops_gfld <- function(op, x, y, ...) {
  UseMethod("Ops_gfld", x)
}

Ops_gfld.geofield <- function(op, x, y) {
  if (missing(y)) {
    Ops_gfld.geofield.MISSING(op, x)
  } else {
    UseMethod("Ops_gfld.geofield", y)
  }
}

Ops_gfld.numeric <- function(op, x, y) {
  UseMethod("Ops_gfld.numeric", y)
}

Ops_gfld.geofield.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

Ops_gfld.geofield.geofield <- function(op, x, y) {
  dom <- get_domain(x)
  if (!check_same_domain(list(dom, get_domain(y)))) {
    abort("geofields must be on the same domain.")
  }
  op <- match.fun(op)
  new_geofield(op(unclass(x), unclass(y)), domain = dom)
}

Ops_gfld.geofield.numeric <- function(op, x, y) {
  dom <- get_domain(x)
  op <- match.fun(op)
  new_geofield(op(unclass(x), y), domain = dom)
}

Ops_gfld.geofield.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

Ops_gfld.numeric.geofield <- function(op, x, y) {
  dom <- get_domain(y)
  op <- match.fun(op)
  new_geofield(op(x, unclass(y)), domain = dom)
}

Ops_gfld.geofield.MISSING <- function(op, x, y) {
  dom <- get_domain(x)
  switch(
    op,
    "-" = x * -1,
    "+" = x,
    "!" = new_geofield(!unclass(x), domain = dom),
    abort(paste(op, "<geofield> is not permitted"))
  )
}

##################
### geolist math #
##################

math_out <- function(x, domain) {
  geolist(new_geofield(x, domain = domain))
}

vec_math.harp_geolist <- function(.fn, .x, na.rm = FALSE, ...) {
  browser()
  dom <- get_domain(.x)
  switch(
    .fn,
    "sum"     = math_out(harpCore:::cpp_geolist_sum(.x, na.rm), dom),
    "prod"    = math_out(harpCore:::cpp_geolist_prod(.x, na.rm), dom),
    "mean"    = math_out(harpCore:::cpp_geolist_mean(.x, na.rm), dom),
    "any"     = math_out(harpCore:::cpp_geolist_any(.x, na.rm), dom),
    "all"     = math_out(harpCore:::cpp_geolist_all(.x, na.rm), dom),
    "cumsum"  = geolist(Reduce(`+`, .x, accumulate = TRUE)),
    "cumprod" = geolist(Reduce(`*`, .x, accumulate = TRUE)),
    "cummin"  = geolist(Reduce(pmin, .x, accumulate = TRUE)),
    "cummax"  = geolist(Reduce(pmax, .x, accumulate = TRUE)),
    glapply(.x, function(x) vec_math_base(.fn, x, ...))
  )
}

min.harp_geolist <- function(x, na.rm = FALSE) {
  domain <- get_domain(x)
  math_out(harpCore:::cpp_geolist_min(x, na.rm), dom)
}

max.harp_geolist <- function(x, na.rm = FALSE) {
  domain <- get_domain(x)
  math_out(harpCore:::cpp_geolist_max(x, na.rm), dom)
}

# var and sd are not generics - need separate functions

variance <- function(x, na.rm = FALSE, ...) {
  UseMethod("variance")
}

#' @export
variance.default <- function(x, na.rm = FALSE, ...) {
  stats::var(x, na.rm = na.rm, ...)
}

#' @export
variance.harp_geolist <- function(x, na.rm = FALSE, ...) {
  dom <- get_domain(x)
  math_out(cpp_geolist_var_sd(x, na.rm), dom)
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
std_dev.harp_geolist <- function(x, na.rm = FALSE, ...) {
  dom <- get_domain(x)
  math_out(cpp_geolist_var_sd(x, na.rm, FALSE), dom)
}

#' @export
diff.harp_geolist <- function(x, lag = 1, trim = TRUE, ...) {
  if (round(lag) != lag) {
    abort("lag must be an integer.")
  }
  if (lag == 0) {
    return(x)
  }
  if (lag > (length(x) - 1)) {
    abort("lag is too long.")
  }
  if (lag > 0) {
    res <- x - dplyr::lag(x, n = lag)
    if (trim) {
      return(res[(lag + 1):length(res)])
    } else {
      return(res)
    }
  }
  res <- x - dplyr::lead(x, n = (lag * -1))
  if (trim) {
    return(res[1:(length(res) + lag)])
  }
  res
}

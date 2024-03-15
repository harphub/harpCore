# vctrs harp_geolist class
#
# Building the harp_geolist class using vctrs - the
# geolist class is a vctrs list_of object. This means
# that many operations for data frames that include a
# harp_geolist column work smoothly, including binding,
# grouped mutates and grouped summaries. Additionally,
# most mathematical and arithmetic operations can be
# handled using the vec_math and vec_arith generics.

#' Internal vctrs methods
#'
#' @import vctrs
#' @keywords internal
#' @name harp-geolist
NULL

######################
### Helper functions #
######################

#' Extract domain information as a geodomain object
#'
#' The "domain" attribute is extracted from the input and returned if it is a
#' geodomain.
#'
#' @param x A geofield or geolist
#'
#' @return A geodomain
#' @export
#'
#' @examples
#' get_domain(det_grid_df$fcst)
#'
#' get_domain(det_grid_df$fcst[[1]])
get_domain <- function(x) {
  UseMethod("get_domain")
}

get_domain_attr <- function(x, call = rlang::caller_env()) {
  dom <- attr(x, "domain")
  if (is.null(dom)) {
    return(NULL)
  }
  if (!meteogrid::is.geodomain(dom)) {
    return(NULL)
  }
  dom
}

#' @export
get_domain.default      <- function(x) get_domain_attr(x)

#' @export
get_domain.geofield     <- function(x) get_domain_attr(x)

#' @export
get_domain.harp_geolist <- function(x) get_domain_attr(x)

#' @export
get_domain.geodomain    <- function(x) x

check_same_domain <- function(x) {
  domains <- lapply(x, get_domain)
  non_null_doms <- which(vapply(domains, function(d) !is.null(d), logical(1)))
  x <- x[non_null_doms]
  if (length(x) < 2) {
    return(TRUE)
  }

  all(
    sapply(
      2:length(x),
      function(i) meteogrid::compare.geodomain(
        get_domain(x[[i]]), get_domain(x[[(i - 1)]])
      )
    )
  )
}

# lapply function for geolists - operates on a geolist and
# returns a geolist
#' Apply a function to each element of a geolist
#'
#' `glapply` is an implementation of \code{\link[base]{lapply}} especially for
#' geolists. `FUN` is applied to each element of the `geolist` and a `geolist`
#' of the same length is returned.
#'
#' @inheritParams base::lapply
#' @return A `geolist` of the same length as `X` with `FUN` applied to all
#'   elements.
#' @export
#'
#' @examples
#' glapply(det_grid_df$fcst, sqrt)
#'
#' glapply(det_grid_df$fcst, function(x) x + 10)
glapply <- function(X, FUN, ...) {
  if (!is_geolist(X)) {
    rlang::abort("`glapply` can only be used on geolists.")
  }
  .f <- function(..., domain) {
    dots <- rlang::list2(...)
    new_geofield(do.call(FUN, dots), domain = domain)
  }
  geolist(lapply(X, .f, domain = get_domain(X), ...))
}

#' Apply a function to element pairs of 2 geolists
#'
#' `glapply2()` is variant of `glapply()` whereby a function that takes 2
#' `geofield`s as arguments is applied element-wise to 2 geolists. It is
#' equivalent to \code{\link[purrr]{map2}}, but is explicitly for `geolist`s.
#'
#' @param X A geolist
#' @param Y A geolist of the same length as `X` that is on the same domain.
#' @param FUN The function to apply to `X` and `Y`.
#' @param ... Other arguments to `FUN`.
#'
#' @return A `geolist` of the same length as `X` and `Y`.
#' @examples
#' glapply2(
#'   ens_grid_df$grid_mbr000, ens_grid_df$grid_mbr001, function(x, y) x + y
#' )
#' @export
glapply2 <- function(X, Y, FUN, ...) {
  if (!(is_geolist(X) && is_geolist(Y))) {
    rlang::abort("`glapply2` can only be used on geolists.")
  }
  if (!meteogrid::compare.geodomain(get_domain(X), get_domain(Y))) {
    rlang::abort("`X` and `Y` must be geolists with the same domain.")
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

#' Apply a funtion that returns a geolist to mutiple list are vector arguments
#'
#' `gmapply()` is an implementation of \code{\link[base]{mapply}()} especially
#' for geolists. `FUN` is applied to the first elements of each `...` argument,
#' the second elements, the third elements and so on. `gmapply()` differs from
#' `mapply()` in that the result is expected to be a `geolist`.
#'
#' @inheritParams base::mapply
#' @return A `geolist` of the same length as the arguments in `...`
#'
#' @export
#'
#' @examples
#' gmapply(
#'   function(a, b, c, d) (a + b * c) / d,
#'   det_grid_df$fcst,
#'   seq_len(nrow(det_grid_df)),
#'   seq_len(nrow(det_grid_df)) * 10,
#'   MoreArgs = list(d = 100)
#' )
gmapply <- function(FUN, ..., MoreArgs = NULL) {

  res <- mapply(
    FUN, ..., MoreArgs = MoreArgs, SIMPLIFY = FALSE, USE.NAMES = FALSE
  )

  if (!all(vapply(res, meteogrid::is.geofield, logical(1)))) {
    cli::cli_abort(c(
      "Not all elements of result are {.cls geofield}s."
    ))
  }

  if (!check_same_domain(res)) {
    cli::cli_abort(c(
      "Not all elements of result are on the same domain."
    ))
  }

  geolist(res)
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

  if (meteogrid::is.geofield(x)) {
    return(x)
  }

  if (!is.null(domain)) {
    if (!meteogrid::is.geodomain(domain)) {
      rlang::abort("`domain` must be a geodomain.")
    }
    if (length(x) != domain[["nx"]] * domain[["ny"]]) {
      rlang::abort("`domain` dimensions do not match dimensions of `x`")
    }
    x <- meteogrid::as.geofield(
      array(x, c(domain[["nx"]], domain[["ny"]])),
      domain = domain
    )
  }

  if (length(x) == 0) {
    x <- array(numeric(), c(0, 0))
  }

  if (!is.numeric(x) && !is.logical(x)) {
    rlang::abort(geofield_error)
  }

  if (length(dim(x)) != 2) {
    rlang::abort(geofield_error)
  }

  if (is.null(domain)) {
    domain <- get_domain(x)
  }

  if (is.null(domain)) {
    return(structure(x, domain = domain, class = "geofield"))
  }
  meteogrid::as.geofield(x, domain = domain)
}

# geofield generic and methods for instantiating a geofield

#' Geofields
#'
#' A geofield is a georeferenced 2d-array. The coordinate reference system is
#' stored as attributes to the array. Used together \code{\link{define_domain}},
#' a geofield can be created from a 2d-array. Additionally, `geofield()` can be
#' used to extract a `geofield` from a \code{\link{geolist}}. This can be
#' useful for extracting a `geofield` in a pipeline.
#'
#' @param x A 2d array or `geolist`
#' @param ... Used for methods
#'
#' @return A `geofield`.
#' @export
#'
#' @examples
#' my_domain <- define_domain(10, 60, 300, 10000)
#' gfld <- geofield(
#'   array(rnorm(300 * 300), c(300, 300)),
#'   domain = my_domain
#' )
#' meteogrid::iview(gfld)
#'
#' geofield(det_grid_df$fcst, 4)
#' # is equivalent to
#' det_grid_df$fcst[[4]]
#' # but geofield() can be used in a pipeline
#' det_grid_df$fcst %>%
#'   geofield(4)
geofield <- function(x, ...) {
  UseMethod("geofield")
}

#' @rdname geofield
#' @param i The element of the `geolist` to extract.
#' @export
geofield.harp_geolist <- function(x, i = 1, ...) {
  new_geofield(x[[i]])
}

#' @rdname geofield
#' @param domain A `geodomain` with the same dimensions as `x`.
#' @export
geofield.array <- function(x, domain, ...) {
  if (missing(domain) || !meteogrid::is.geodomain(domain)) {
    rlang::abort("`domain` must be passed as a geodomain.")
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
validate_geolist <- function(x, call = rlang::caller_env()) {
  if (length(x) < 1) {
    return(x)
  }
  valid_indices <- which(
    sapply(x, function(.x) length(.x) > 0)
  )
  if (length(valid_indices) < 1) {
    return(x)
  }
  if (!all(sapply(x[valid_indices], meteogrid::is.geofield))) {
    rlang::abort("All elements of a geolist must be geofields.", call = call)
  }
  if (!check_same_domain(x[valid_indices])) {
    rlang::abort(
      "All geofields in a geolist must be on the same domain.", call = call
    )
  }
  # if (!length(unique(sapply(x[valid_indices], mode))) == 1) {
  #   rlang::abort("All geofields must be the same type.", call = call)
  # }
  if (
    !is.null(get_domain(x)) &&
    !identical(get_domain(x), get_domain(x[valid_indices][[1]]))
  ) {
    rlang::abort("Domain mismatch between geolist and geofields.", call = call)
  }
  x
}

# User facing instantiator

#' Geolists
#'
#' A `geolist` is a list of \code{\link{geofield}s} that are on the same domain.
#' The main purpose of a `geolist` is to be used as a column in a data frame.
#'
#' A number of methods are available for `geolist`s that reduce the `geolist` to
#' a `geolist` containing a single `geofield`. These include:
#'
#' * `mean` - element-wise mean of the `geolist`
#' * `std_dev` - element-wise standard deviation of the `geolist`
#' * `variance` element-wise variance of the `geolist`
#' * `min` - element-wise minimum of the `geolist`
#' * `max` - element-wise maximum of the `geolist`
#' * `cumsum` - element-wise cumulative sum of the `geolist`
#' * `cumprod` - element-wise cumulative product of the `geolist`
#' * `cummin` - element-wise cumulative minimum of the `geolist`
#' * `cummax` - element-wise cumulative maximum of the `geolist`
#' * `any` - element-wise any of a logical `geolist` is TRUE
#' * `all` - element-wise all of a logical `geolist` is TRUE
#'
#' Note that the R stats functions \code{\link[stats]{sd}} and
#' \code{\link[stats]{var}} cannot be used on `geolist`s since they are not
#' implemented as methods, so `std_dev` and `variance` must be used instead.
#'
#' In addition most of R's generic math and logic functions work with
#' `geolist`s.
#'
#' `is_geolist()` checks if the argument is a valid geolist.
#'
#' @param ... geofields or a list containing geofields
#' @param domain Typically not used, since the domain will be obtained from the
#'   geofields
#'
#' @return A list of geofields on the same domain with class `harp_geolist`
#' @export
#'
#' @examples
#' # Define a domain
#' my_domain <- define_domain(10, 60, 300, 10000)
#'
#' # geolist from indivdual geofields
#' geolist(
#'   geofield(array(rnorm(300 * 300), c(300, 300)), domain = my_domain),
#'   geofield(array(rnorm(300 * 300), c(300, 300)), domain = my_domain)
#' )
#'
#' # geolist from a list of geofields
#' gfld <- lapply(
#'   1:10,
#'   function(x) geofield(
#'     array(runif(300 * 300), c(300, 300)), domain = my_domain
#'   )
#' )
#' glst <- geolist(gfld)
#' glst
#'
#' # Summarise geolist to a single geofield
#' mean(glst)
#' std_dev(glst)
#' variance(glst)
#' min(glst)
#' max(glst)
#' any(glst > 0.9)
#' all(glst > 0.25)
geolist <- function(..., domain = NULL) {
  x <- list(...)
  if (length(x) == 1 && is.list(x[[1]])) {
    x <- x[[1]]
  }
  validate_geolist(new_geolist(x, domain = domain))
}

#' @rdname geolist
#' @param x An object
#' @export
is_geolist <- function(x) {
  inherits(x, "harp_geolist")
}

# Formatting for geolists
#' @export
print.harp_geolist <- function(x, n = 10, ...) {
  obj_print_header(x, ...)
  obj_print_data(x, n, ...)
  obj_print_footer(x, n, ...)
  invisible(x)
}

print_data_harp_geolist <- function(x, ...) {
  format_one <- function(i, x, ws) {
    ws <- ws - nchar(i)
    index_txt <- paste0(paste(rep(" ", ws), collapse = ""), "[[", i, "]]")
    if (length(x) == 0) {
      txt <- paste(index_txt, "<empty>\n")
    } else if (!inherits(x, "geofield")) {
      txt <- paste(index_txt, "<not_a_geofield>\n")
    } else {
      dom <- get_domain(x)
      txt <- paste0(
        " geofield [",
        dom$nx,
        " x ",
        dom$ny, "] "
      )
      num_na <- sum(is.na(x))
      if (num_na < 1) {
        na_txt <- ""
      } else {
        na_txt <- paste(" NAs:", num_na)
      }
      if (is.numeric(x)) {
        min_x  <- min(x, na.rm = TRUE)
        max_x  <- max(x, na.rm = TRUE)
        mean_x <- mean(x, na.rm = TRUE)
        min_txt  <- pillar::style_subtle_num(sprintf("%.3f", min_x), min_x < 0)
        max_txt  <- pillar::style_subtle_num(sprintf("%.3f", max_x), max_x < 0)
        mean_txt <- pillar::style_subtle_num(sprintf("%.3f", mean_x), mean_x < 0)
        txt <- paste0(
          index_txt,
          " <numeric", txt,
          "Min = ", min_txt,
          " Max = ", max_txt,
          " Mean = ", mean_txt,
          na_txt,
          ">\n"
        )
      } else if (is.logical(x)) {
        num_true  <- sum(x, na.rm = TRUE)
        num_false <- sum(!x[!x], na.rm = TRUE)
        true_txt  <- num_true
        false_txt <- pillar::style_subtle_num(num_false, num_false > 0)
        txt <- paste0(
          index_txt,
          " <logical", txt,
          "TRUE: ", true_txt,
          " FALSE: ", false_txt,
          na_txt,
          ">\n"
        )
      } else {
        txt <- paste0(index_txt, "<", txt, ">\n")
      }
    }
    cat(txt)
  }
  max_txt_length <- nchar(length(x))
  invisible(
    mapply(format_one, seq_along(x), x, MoreArgs = list(ws = max_txt_length))
  )
}

#' @export
obj_print_data.harp_geolist <- function(x, n = 10, ...) {
  if (length(x) != 0) {
    if (length(x) <= n) {
      n <- length(x)
    }
    print_data_harp_geolist(x[1:n])
  }
}

#' @export
obj_print_footer.harp_geolist <- function(x, n = 10, ...) {
  if (length(x) <= n) {
    return()
  }
  xtra <- length(x) - n
  cat(pillar::style_subtle(paste("#", xtra, "more geofields\n")))
  cat(pillar::style_subtle("# Use `print(n = ...)` to see more\n"))
}

#' @export
obj_print_header.harp_geolist <- function(x, ...) {
  cat(pillar::style_subtle(paste0("<harp_geolist[", length(x), "]>\n")))
}

#' @export
format.harp_geolist <- function(x, ...) {
  empties <- which(sapply(x, length) < 1)
  non_empties <- setdiff(seq_along(x), empties)
  out <- character(length(x))
  out[non_empties] <- sapply(
    x[non_empties],
    function(.x) paste("<geofield>", paste(dim(.x), collapse = " x "))
  )
  out[empties] <- "<empty>"
  out
}

# Formatting for geolists in data frame column
# #' @export
# pillar_shaft.harp_geolist <- function(x, ...) {
#   out <- format(x, formatter = geolist_formatter)
#   pillar::new_pillar_shaft_simple(out)
#   # format_row <- function(x) {
#   #   if (length(x) == 0) {
#   #     x <- "<empty>"
#   #   } else if (!inherits(x, "geofield")) {
#   #     x <- "<not_a_geofield>"
#   #   } else {
#   #     dom <- get_domain(x)
#   #     x <- paste0("<gfld [", dom$nx, " x ", dom$ny, "]>")
#   #   }
#   #   pillar::style_subtle(x)
#   # }
#   # pillar::new_pillar_shaft_simple(
#   #   vapply(x, format_row, character(1))
#   # )
# }

# Type abbreviation for data frame headings
#' @export
vec_ptype_abbr.harp_geolist <- function(x, ...) "geolist"

##########################
### Casting and coercion #
##########################

#Coercion
ptype2_check_domains <- function(X) {
  if (!check_same_domain(X)) {
    stop("geolist domains are not the same", call. = FALSE)
  }
}

#' @export
vec_ptype2.harp_geolist.harp_geolist <- function(x, y, ...) {
  ptype2_check_domains(list(x, y))
  geolist(domain = get_domain(x))
}

#' @export
vec_ptype2.harp_geolist.geofield <- function(x, y, ...) {
  ptype2_check_domains(list(x, y))
  y <- geolist(y)
  geolist(domain = get_domain(x))
}

#' @export
vec_ptype2.geofield.harp_geolist <- function(x, y, ...) {
  ptype2_check_domains(list(x, y))
  x <- geolist(x)
  geolist(domain = get_domain(x))
}

#' @export
vec_ptype2.geofield.geofield <- function(x, y, ...) {
  ptype2_check_domains(list(x, y))
  geolist(domain = get_domain(x))
}

# Casting

#' @export
vec_cast.geofield.geofield <- function(x, to, ...) x

#' @export
vec_cast.harp_geolist.harp_geolist <- function(x, to, ...) x

#' @export
vec_cast.harp_geolist.geofield <- function(x, to, ...) geolist(x)

##############################
# Concatenation of geofields #
##############################

c.geofield <- function(...) geolist(list(...))

#############################################
# geofield Ops methods with double dispatch #
#############################################

# Ensure mathematical operations with geofields return geofields
# This should possibly be in meteogrid?

#' @export
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

Ops_gfld.array <- function(op, x, y) {
  UseMethod("Ops_gfld.array", y)
}

Ops_gfld.geofield.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

Ops_gfld.geofield.geofield <- function(op, x, y) {
  dom <- get_domain(x)
  if (!check_same_domain(list(dom, get_domain(y)))) {
    rlang::abort("geofields must be on the same domain.")
  }
  op <- match.fun(op)
  new_geofield(op(unclass(x), unclass(y)), domain = dom)
}

Ops_gfld.geofield.numeric <- function(op, x, y) {
  if (length(y) > 1) {
    stop_incompatible_op(
      op, x, y,
      details = "for <{vec_ptype_full(y)}> of length > 1"
    )
  }
  dom <- get_domain(x)
  op <- match.fun(op)
  new_geofield(op(unclass(x), y), domain = dom)
}

Ops_gfld.geofield.NULL <- function(op, x, y) {
  new_geofield()
}

Ops_gfld.geofield.default <- function(op, x, y) {
  stop_incompatible_op(op, x, y)
}

Ops_gfld.geofield.array <- function(op, x, y) {
  if (!identical(dim(x), dim(y))) {
    stop_incompatible_op(op, x, y)
  } else {
    op <- match.fun(op)
    dom <- get_domain(x)
    new_geofield(op(unclass(x), y))
  }
}

Ops_gfld.array.geofield <- function(op, x, y) {
  if (!identical(dim(x), dim(y))) {
    stop_incompatible_op(op, x, y)
  } else {
    op <- match.fun(op)
    dom <- get_domain(y)
    new_geofield(op(x, unclass(y)))
  }
}

Ops_gfld.numeric.geofield <- function(op, x, y) {
  if (length(x) > 1) {
    stop_incompatible_op(
      op, x, y,
      details = "for <{vec_ptype_full(x)}> of length > 1"
    )
  }
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
    rlang::abort(paste(op, "<geofield> is not permitted"))
  )
}

#' @export
is.na.geofield <- function(x) {
 dom <- get_domain(x)
 new_geofield(is.na(unclass(x)), domain = dom)
}

##################
### geolist math #
##################

#' @export
is.na.harp_geolist <- function(x) {
  glapply(x, is.na)
}

math_out <- function(x, domain) {
  geolist(new_geofield(x, domain = domain))
}

#' @export
vec_math.harp_geolist <- function(.fn, .x, na.rm = FALSE, ...) {
  dom <- get_domain(.x)
  switch(
    .fn,
    "sum"     = math_out(cpp_geolist_sum(.x, na.rm), dom),
    "prod"    = math_out(cpp_geolist_prod(.x, na.rm), dom),
    "mean"    = math_out(cpp_geolist_mean(.x, na.rm), dom),
    "any"     = math_out(cpp_geolist_any(.x, na.rm), dom),
    "all"     = math_out(cpp_geolist_all(.x, na.rm), dom),
    "cumsum"  = geolist(Reduce(`+`, .x, accumulate = TRUE)),
    "cumprod" = geolist(Reduce(`*`, .x, accumulate = TRUE)),
    "cummin"  = geolist(Reduce(pmin, .x, accumulate = TRUE)),
    "cummax"  = geolist(Reduce(pmax, .x, accumulate = TRUE)),
    glapply(.x, function(x) vec_math_base(.fn, x, ...))
  )
}

# min and max are not part of vec_math

#' @export
min.harp_geolist <- function(..., na.rm = FALSE) {
  x <- list(...)[[1]]
  domain <- get_domain(x)
  math_out(cpp_geolist_min(x, na.rm), domain)
}

#' @export
max.harp_geolist <- function(..., na.rm = FALSE) {
  x <- list(...)[[1]]
  domain <- get_domain(x)
  math_out(cpp_geolist_max(x, na.rm), domain)
}

# var and sd are not generics - need separate functions

#' @rdname geolist
#' @export
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

#' @rdname geolist
#' @param na.rm Logical. Whether to remove `NA`s before calculation.
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
    rlang::abort("`lag` must be an integer.")
  }
  if (lag == 0) {
    return(x)
  }
  if (lag > (length(x) - 1)) {
    rlang::abort("`lag` is too long.")
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

########################
### geolist arithmetic #
########################

# Need to set a method for Ops to separate out arithmetic and comparison

#' @export
Ops.harp_geolist <- function(e1, e2) {
  switch(
    .Generic,
    "+"   = ,
    "-"   = ,
    "/"   = ,
    "*"   = ,
    "^"   = ,
    "%%"  = ,
    "%/%" = ,
    "!"   = ,
    "&"   = ,
    "|"   = NextMethod(),
    harp_geolist_compare(.Generic, e1, e2)
  )
}

# Arithmetic

#' @export
#' @method vec_arith harp_geolist
vec_arith.harp_geolist <- function(op, x, y, ...) {
  UseMethod("vec_arith.harp_geolist", y)
}

#' @export
vec_arith.harp_geolist.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

#' @export
#' @method vec_arith.harp_geolist harp_geolist
vec_arith.harp_geolist.harp_geolist <- function(op, x, y, ...) {
  if (length(x) != length(y)) {
    stop_incompatible_op(
      op, x, y,
      message = glue::glue(
        "<{vec_ptype_full(x)}[{length(x)}]> {op} <{vec_ptype_full(y)}[{length(y)}]> is not permitted"
      )
    )
  }
  glapply2(x, y, op, ...)
}

#' @export
#' @method vec_arith.harp_geolist numeric
vec_arith.harp_geolist.numeric <- function(op, x, y, ...) {
  glapply(x, function(z) vec_arith_base(op, z, y, ...))
}

#' @export
#' @method vec_arith.numeric harp_geolist
vec_arith.numeric.harp_geolist <- function(op, x, y, ...) {
  glapply(y, function(z) vec_arith_base(op, z, x, ...))
}

# Comparison

harp_geolist_compare <- function(op, x, y, ...) {
  UseMethod("harp_geolist_compare", x)
}

harp_geolist_compare.harp_geolist <- function(op, x, y, ...) {
  UseMethod("harp_geolist_compare.harp_geolist", y)
}

harp_geolist_compare.harp_geolist.default <- function(op, x, y, ...) {
  stop_incompatible_op(op, x, y)
}

harp_geolist_compare.harp_geolist.harp_geolist <- function(op, x, y, ...) {
  glapply2(x, y, op, ...)
}

harp_geolist_compare.harp_geolist.numeric <- function(op, x, y, ...) {
  glapply(x, op, y, ...)
}

harp_geolist_compare.numeric <- function(op, x, y, ...) {
  UseMethod("harp_geolist_compare.numeric", y)
}

harp_geolist_compare.numeric.harp_geolist <- function(op, x, y, ...) {
  op <- match.fun(op)
  glapply(y, function(.y) op(x, .y), ...)
}

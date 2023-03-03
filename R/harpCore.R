## usethis namespace: start
#' @useDynLib harpCore, .registration = TRUE
## usethis namespace: end
## usethis namespace: start
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
.onUnload <- function(libpath) {
  library.dynam.unload("harpCore", libpath)
}


#' Data for examples
#'
#' Data used for demonstrating function usage in examples. The data are random
#' numbers sampled from a uniform distribution between 0 and 1.
#'
#' @name example_data
#' @keywords datasets
#' @format A \code{harp_df} data frame or \code{harp_list}
#' \describe{
#'   \item{fcdate}{Forecast start date-time}
#'   \item{lead_time}{Forecast lead time in hours}
#'   \item{validdate}{Forecast valid date-time}
#'   \item{SID}{ID of observation station}
#'   \item{point_det}{Deterministic point forecast value}
#'   \item{point_mbr000}{Point forecast value for ensemble member 000}
#'   \item{point_mbr001}{Point forecast value for ensemble member 001}
#'   \item{grid_det}{Deterministic gridded forecast}
#'   \item{grid_mbr000}{Gridded forecast for ensemble member 000}
#'   \item{grid_mbr001}{Gridded forecast for ensemble member 001}
#' }
"det_point_df"

#' @rdname example_data
#' @format NULL
"ens_point_df"

#' @format NULL
#' @rdname example_data
"det_grid_df"

#' @format NULL
#' @rdname example_data
"ens_grid_df"

#' @format NULL
#' @rdname example_data
"det_point_list"

#' @format NULL
#' @rdname example_data
"ens_point_list"

#' @format NULL
#' @rdname example_data
"det_grid_list"

#' @format NULL
#' @rdname example_data
"ens_grid_list"

#' Data for examples
#'
#' Data used for demonstrating function usage in examples. The data are random
#' numbers sampled from a uniform distribution between 0 and 1.
#'
#' @name example_data
#' @keywords datasets
#' @format A \code{harp_df} data frame or \code{harp_list}
#' \describe{
#'   \item{fcst_dttm}{Forecast start date-time}
#'   \item{lead_time}{Forecast lead time in hours}
#'   \item{valid_dttm}{Forecast valid date-time}
#'   \item{SID}{ID of observation station}
#'   \item{point_det}{Deterministic point forecast value}
#'   \item{point_mbr000}{Point forecast value for ensemble member 000}
#'   \item{point_mbr001}{Point forecast value for ensemble member 001}
#'   \item{grid_det}{Deterministic gridded forecast}
#'   \item{grid_mbr000}{Gridded forecast for ensemble member 000}
#'   \item{grid_mbr001}{Gridded forecast for ensemble member 001}
#'   \item{grid_anl}{Gridded analysis}
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

#' @format NULL
#' @rdname example_data
"anl_grid_df"

#' List of Weather Stations
#'
#' A dataset containing site ID, latitude, longitude, elevation and name of
#' 13417 weather stations world wide.
#'
#' @format A data frame with 13417 rows and 5 variables: \describe{
#'   \item{SID}{station ID number} \item{lat}{latitude of the station, in
#'   decimal degrees} \item{lon}{longitude of the station, in decimal degrees}
#'   \item{elev}{elevation of the station, in metres} \item{name}{the name of
#'   the station}}
#'
#' @source HIRLAM station list
"station_list"

#' Weather station IDs with regional groups
#'
#' A dataset with station IDs and geographic groups they belong it. Can be used
#' to do grouped verification by joining the station_groups to a harp_fcst list
#' and running the verification with \code{groupings = c("leadtime", "group")}.
#' Note that many stations belong to more than one group
#'
#' @format A data frame with 6001 rows and 2 variables: \describe{
#'   \item{SID}{station ID number}
#'   \item{station_group}{Geographic group the station belongs to}
#' }
#'
#' @source selection.pm from HARMONIE / HIRLAM monitor
"station_groups"

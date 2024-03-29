% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utility_functions.R
\name{make_verif_groups}
\alias{make_verif_groups}
\title{Combine groups to make a list of all possible verification groups}
\usage{
make_verif_groups(time_groups, groups)
}
\arguments{
\item{time_groups}{Groups that could be used as a time axis in a plot. Can be
any combination of "lead_time", "valid_dttm" and "valid_hour".}

\item{groups}{Other group columns to use in a verification.}
}
\value{
A list of group combinations
}
\description{
This function is intended to define all possible group combinations for use
in verification functions such as \code{\link[harpPoint]{det_verify}}. When
\code{time_groups} are given, the function ensures that they appear in every
group.
}
\examples{
make_verif_groups("lead_time", c("fcst_cycle", "station_group"))
make_verif_groups(
  c("lead_time", "valid_dttm", "valid_hour"),
  c("fcst_cycle", "station_group")
)
}

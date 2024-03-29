% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harp_df_list.R
\name{ens_mean_and_var}
\alias{ens_mean_and_var}
\title{Compute the ensemble mean and variance}
\usage{
ens_mean_and_var(
  .fcst,
  mean_name = "ens_mean",
  var_name = "ens_var",
  sd_name = "ens_spread",
  var_drop_member = NULL
)
}
\arguments{
\item{.fcst}{A \code{harp_list} object, or a \code{harp_ens_point_df} or
\code{harp_ens_grid_df} data frame.}

\item{mean_name}{The output column name for the ensemble mean}

\item{var_name}{The output column name for the ensemble variance}

\item{sd_name}{The output column name for the ensemble spread (standard
deviation)}

\item{var_drop_member}{Which members to drop for the calculation of the
ensemble variance and standard deviation. For harp_fcst objects, this can
be a numeric scalar - in which case it is recycled for all forecast models;
a list or numeric vector of the same length as the harp_fcst object, or a
named list with the names corresponding to names in the harp_fcst object.}

\item{...}{}
}
\value{
A \code{harp_fcst} object with columns ens_mean and ens_var added to
the forecast tables.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#superseded}{\figure{lifecycle-superseded.svg}{options: alt='[Superseded]'}}}{\strong{[Superseded]}} This function is superseded by
\code{\link[harpCore]{ens_stats}}. However, \code{ens_mean_and_var()} is still
useful for computing the ensemble spread with a dropped member and is used by
\code{\link[harpPoint]{ens_spread_and_skill}}.
}
\details{
The ensemble mean and variance are computed and added as columns to tables in
a \code{harp_df} object or \code{harp_list} object..
}

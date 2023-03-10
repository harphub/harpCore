% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harp_df_list.R
\name{unique_col}
\alias{unique_col}
\alias{unique_stations}
\alias{unique_valid_dttm}
\alias{unique_fcst_dttm}
\title{Extract unique values from a data frame column}
\usage{
unique_col(.data, col)

unique_stations(.data)

unique_valid_dttm(.data)

unique_fcst_dttm(.data)
}
\arguments{
\item{.data}{A data frame or harp_list}

\item{col}{The column from which to extract the unique values. Can be quoted
or unquoted.}
}
\value{
A vector of unique values.
}
\description{
Unique values are extracted from the named column in a data frame. In the
case of a \code{harp_list}, unique values across all of the data frames
in the list are extracted as a single vector. If a column is not found,
a warning is issued and NULL is returned.
}
\details{
\code{unique_stations}, \code{unique_fcst_dttm} and \code{unique_valid_dttm}
are wrappers around \code{unique_col} that extract the unique stations,
valid date-time and forecast start date-time using the standard harp
column names "SID", "valid_dttm" and "fcst_dttm" respectively.
}
\examples{
unique_col(det_point_df, fcst_dttm)
unique_col(det_point_df, SID)

# Works with quoted column names too
unique_col(det_point_df, "SID")

# Use {{<var>}} for variables as columns
my_col <- "SID"
unique_col(det_point_df, {{my_col}})
}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/harp_df_list.R
\name{pivot_members}
\alias{pivot_members}
\title{Pivot between wide and long ensemble data frames}
\usage{
pivot_members(.data)
}
\arguments{
\item{.data}{A harp data frame or a \code{harp_list} of data frames}
}
\value{
The same data frame, or harp_list but with the members pivoted.
}
\description{
The default behaviour in harp is to store ensemble data in wide data frames.
That means that there is one column for each member of the ensemble. This
isn't always ideal and goes against the principles of tidy data, whereby
each ensemble member would be stored on a separate row with a single
column denoting the ensemble member. \code{pivot_members} can be used to
pivot between the wide and long formats in both directions.
}
\details{
When pivoting from a wide to a long data frame, the class is updated to
indicate that the ensemble members are stored in rows rather than
columns. When pivoting back to a wide data frame format, the class is
returned to its original names.
}
\examples{
pivot_members(ens_point_df)
pivot_members(ens_grid_df)
pivot_members(ens_point_list)
pivot_members(ens_grid_list)

# Note the change in class
class(ens_point_df)
class(pivot_members(ens_point_df))
class(ens_grid_df)
class(pivot_members(ens_grid_df))
}

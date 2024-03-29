% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_weights.R
\name{geo_weights}
\alias{geo_weights}
\alias{geo_weights_points}
\alias{geo_weights_regrid}
\alias{geo_weights_xsection}
\title{Compute interpolation weights for geographic transformations}
\usage{
geo_weights(x, trans = c("points", "regrid", "xsection"), opts)

geo_weights_points(
  x,
  points,
  method = c("bilinear", "nearest", "bicubic"),
  mask = NULL,
  force = FALSE
)

geo_weights_regrid(
  x,
  new_grid,
  method = c("bilinear", "nearest", "bicubic"),
  mask = NULL,
  new_mask = NULL
)

geo_weights_xsection(
  x,
  p1,
  p2,
  n = 100,
  method = c("bilinear", "nearest", "bicubic")
)
}
\arguments{
\item{x}{A geofield, geolist, or a data frame with class \code{harp_grid_df}. For
transformations that do not involve the interpolation of gridded data (e.g.
zoom, subgrid) \code{x} can also be a geodomain.}

\item{trans}{The transformation to apply. Can be "points", "regrid",
"xsection", "subgrid", or "zoom".}

\item{opts}{A list of options for the chosen transformation. The appropriate
\link{geo_opts} function should be used to generate this list.}

\item{points}{A data frame of geographic points to which to interpolate the
gridded data. The data frame must include the columns "SID" for a unique id
for the point, "lon" for the longitude of the point in decimal degrees and
"lat" for the latitude of the point in decimal degrees. The data frame can
contain other columns, which will be retained in the output.}

\item{method}{The interpolation method. Can be "nearest" for nearest
neighbour, "bilinear", or "bicubic." The default is "bilinear". For
\code{geo_upscale}, can be any function that summarises a vector to a
single value and can found with \code{\link[base]{match.fun}}, the default
being "mean". A further option is "downsample", dwhich is described in the
argument for \code{downsample_location}.}

\item{mask}{A mask to prevent grid points being used in the interpolation.
Should be on the same grid as \code{x} and grid points with values of 0 or FALSE
will be masked from the interpolation.}

\item{force}{For interpolations that include a mask, it is possible that a
point will be surrounded by 4 masked points. In this case the mask will be
ignored and all 4 points used in the interpolation (the default). Set
\code{force = TRUE} to force the mask to be applied and set the interpolated to
\code{NA}.}

\item{new_grid}{A \code{geofield} or \code{geodomain} on the grid that \code{x} should be
regridded to. \link{define_domain} can be used to define a new \code{geodomain}.}

\item{new_mask}{A \code{geofield} on the same grid as \code{new_grid} with grid points
that should not be interpolated to set to 0 or FALSE.}

\item{p1}{The geographic location in decimal degrees of the start of the
section. Should be a vector of length 2 with the first value being the
longitude and the second value the latitude.}

\item{p2}{The geographic location in decimal degrees of the end of the
section. Should be a vector of length 2 with the first value being the
longitude and the second value the latitude.}

\item{n}{The number of equally spaced points along the section. The default
is 100.}
}
\value{
The interpolation weights for the geographic transformation.
}
\description{
For repeated transformations from the same grid definition, it may be more
efficient to compute the interpolation weights first and then compute the
transformations with those weights. These functions are used to compute the
interpolation weights, which can then be passed to the \code{geo_transform}
function via the \code{opts} argument, or to an explicit \verb{geo_<transformation>}
function via the \verb{weights argument}
}

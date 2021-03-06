% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chull.R
\name{chullPerimeter}
\alias{chullPerimeter}
\title{Perimeter of the convex hull of a set of points}
\usage{
chullPerimeter(x, y, geo = FALSE)
}
\arguments{
\item{x}{A vector of x (or longitude) coordinates.}

\item{y}{A vector of y (or latitude) coordinates.}

\item{geo}{A logical value indicating whether the locations are defined by 
geographic coordinates (pairs of longitude/latitude values). If \code{TRUE}, 
the perimeter is returned as meters. If \code{FALSE}, it is returned as 
units of the \code{[x,y]} coordinates. Default: \code{FALSE}.}
}
\value{
A single numeric value corresponding to the perimeter of the convex 
 hull (in meters if \code{geo} is \code{TRUE}).
}
\description{
Given a set of cartesian coordinates, this function determines 
 the perimeter of the convex hull (or envelope) of the set.
}
\examples{
# TODO

}
\author{
Simon Garnier, \email{garnier@njit.edu}
}

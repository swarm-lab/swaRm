#' @title Is a point part of the convex hull of a set of points?
#'
#' @description Given a set of cartesian coordinates, this function determines 
#'  which points belongs to the convex hull (or envelope) of the set. 
#' 
#' @param x A vector of x (or longitude) coordinates. 
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @return A numerical vector of the same length as \code{x} and \code{y}. 
#'  \code{0} indicates that the corresponding point is not part of the convex 
#'  hull of the set. Values \code{>0} indicates that the corresponding point is 
#'  part of the convex hull, and each value corresponds to the order of the 
#'  point along the convex hull polygon.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
isChull <- function(x, y) {
  if (length(x) != length(y)) 
    stop("x and y should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  if (all(is.na(x)) | all(is.na(y)))
    stop("All x and/or y are NAs.")
  
  idx <- is.na(x) | is.na(y)
  x[idx] <- mean(x, na.rm = TRUE)
  y[idx] <- mean(y, na.rm = TRUE)
  ch <- grDevices::chull(x, y)
  pos <- 1:length(ch)
  is.chull <- rep(FALSE, length(x))
  is.chull[ch] <- pos
  is.chull
}


#' @title Surface area of the convex hull of a set of points
#'
#' @description Given a set of cartesian coordinates, this function determines 
#'  the surface area of the convex hull (or envelope) of the set. 
#' 
#' @param x A vector of x (or longitude) coordinates. 
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). If \code{TRUE}, 
#'  the surface area is returned as square meters. If \code{FALSE}, it is 
#'  returned as square units of the \code{[x,y]} coordinates. Default: 
#'  \code{FALSE}. 
#' 
#' @return A single numeric value corresponding to the surface area of the 
#'  convex hull (in square meters if \code{geo} is \code{TRUE}).
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
chullArea <- function(x, y, geo = FALSE) {
  ch <- isChull(x, y)
  idx <- which(ch > 0)
  ord <- sort(ch[ch > 0], index.return = TRUE)$ix
  
  xx <- x[idx][ord]
  xx <- c(xx, xx[1])
  yy <- y[idx][ord]
  yy <- c(yy, yy[1])
  
  if (geo) {
    geosphere::areaPolygon(cbind(xx, yy))
  } else {
    splancs::areapl(cbind(xx, yy))
  }
}


#' @title Perimeter of the convex hull of a set of points
#'
#' @description Given a set of cartesian coordinates, this function determines 
#'  the perimeter of the convex hull (or envelope) of the set. 
#' 
#' @param x A vector of x (or longitude) coordinates. 
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). If \code{TRUE}, 
#'  the perimeter is returned as meters. If \code{FALSE}, it is returned as 
#'  units of the \code{[x,y]} coordinates. Default: \code{FALSE}. 
#' 
#' @return A single numeric value corresponding to the perimeter of the convex 
#'  hull (in meters if \code{geo} is \code{TRUE}).
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
chullPerimeter <- function(x, y, geo = FALSE) {
  ch <- isChull(x, y)
  idx <- which(ch > 0)
  ord <- sort(ch[ch > 0], index.return = TRUE)$ix
  
  xx <- x[idx][ord]
  xx <- c(xx, xx[1])
  yy <- y[idx][ord]
  yy <- c(yy, yy[1])
  
  if (geo) {
    geosphere::perimeter(cbind(xx, yy))
  } else {
    .cartesianPerimeter(xx, yy)
  }
}

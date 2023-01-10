#' @title Points on Convex Hull
#'
#' @description Given a set of locations, this function determines which 
#'  locations belongs to the convex hull (or envelope) of the set. 
#' 
#' @param x A vector of x (or longitude) coordinates. 
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @return A numerical vector of the same length as \code{x} and \code{y}. 
#'  \code{0} indicates that the corresponding location is not part of the convex 
#'  hull of the set. Values \code{>0} indicates that the corresponding location 
#'  is part of the convex hull, and each value corresponds to the order of the 
#'  locations along the convex hull polygon.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{chull_area}}, \code{\link{chull_perimeter}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' is_chull(x, y)
#' 
#' @export
is_chull <- function(x, y) {
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

#' @rdname is_chull
#' @export
isChull <- function(x, y) {
  .Deprecated("is_chull")
  is_chull(x, y)
}


#' @title Surface Area of the Convex Hull
#'
#' @description Given a set of locations, this function determines the surface 
#'  area of the convex hull (or envelope) of the set. 
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
#' @seealso \code{\link{is_chull}}, \code{\link{chull_perimeter}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' chull_area(x, y)
#' 
#' @export
chull_area <- function(x, y, geo = FALSE) {
  ch <- is_chull(x, y)
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

#' @rdname chull_area
#' @export
chullArea <- function(x, y, geo = FALSE) {
  .Deprecated("chull_area")
  chull_area(x, y, geo)
}


#' @title Perimeter of the Convex Hull
#'
#' @description Given a set of locations, this function determines the perimeter 
#'  of the convex hull (or envelope) of the set. 
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
#' @seealso \code{\link{is_chull}}, \code{\link{chull_area}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' chull_perimeter(x, y)
#' 
#' @export
chull_perimeter <- function(x, y, geo = FALSE) {
  ch <- is_chull(x, y)
  idx <- which(ch > 0)
  ord <- sort(ch[ch > 0], index.return = TRUE)$ix
  
  xx <- x[idx][ord]
  xx <- c(xx, xx[1])
  yy <- y[idx][ord]
  yy <- c(yy, yy[1])
  
  if (geo) {
    geosphere::perimeter(cbind(xx, yy))
  } else {
    .cartesian_perimeter(xx, yy)
  }
}

#' @rdname chull_perimeter
#' @export
chullPerimeter <- function(x, y, geo = FALSE) {
  .Deprecated("chull_perimeter")
  chull_perimeter(x, y, geo)
}

#' @title Perimeter Of A Polygon In Cartesian Space
#' 
#' @description Given a set of Cartesian coordinates representing a polygon, 
#'  this function computes the perimeter of the polygon.
#' 
#' @param x A vector of x coordinates. 
#' 
#' @param y A vector of y coordinates.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{chull_perimeter}}
.cartesian_perimeter <- function(x, y) {
  l <- length(x)
  d <- sqrt((x[1:(l - 1)] - x[2:l]) ^ 2 + (y[1:(l - 1)] - y[2:l]) ^ 2)
  sum(d)
}

#' @rdname rot_order
#' @export
.cartesianPerimeter <- function(x, y) {
  .Deprecated(".cartesian_perimeter")
  .cartesian_perimeter(x, y)
}


#' @title Bivariate Confidence Ellipse
#' 
#' @description This function computes the confidence ellipse of a set of 
#'  bivariate coordinates. 
#' 
#' @param x A vector of x coordinates. 
#' 
#' @param y A vector of y coordinates.
#' 
#' @param level The confidence level of the ellipse. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{sphericity}}, \code{\link{stretch}}
.ellipse <- function(x, y, level = 0.95) {
  idx <- is.na(x) | is.na(y)
  x <- x[!idx]
  y <- y[!idx]
  
  n <- length(x)
  M <- cbind(x, y)
  
  tmp <- MASS::cov.trob(M, maxit = 200)
  
  eig <- eigen(tmp$cov)
  eigval <- eig$values
  eigvec <- eig$vectors
  eigidx <- order(eigval)
  
  qfval <- stats::qf(level, 2, n - 1)
  
  if (eigidx[1] == 1) {
    a = sqrt(2 * eigval[2] * qfval)
    b = sqrt(2 * eigval[1] * qfval)
  } else {
    a = sqrt(2 * eigval[1] * qfval);
    b = sqrt(2 * eigval[2] * qfval);
  }
  
  alpha <- atan(eigvec[2, 1] / eigvec[2, 2])
  
  list(xC = tmp$center[1], yC = tmp$center[2], alpha = alpha, a = a, b = b)
}

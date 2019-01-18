#' @title Compute The Mode(s) Of A Discrete Distribution
#' 
#' @description This is an internal utility function to compute the mode(s) of 
#'  a discrete distribution.
#' 
#' @param x A vector or matrix of discrete values. 
#' 
#' @param na.rm A logical value indicating whether NA values should be stripped 
#'  before the computation proceeds (default: TRUE).
#' 
#' @return A vector of values corresponding to the mode(s) of x.  
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
.Mode <- function(x, na.rm = TRUE) {
  if (na.rm) {
    x <- x[!is.na(x)]
  }
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}


#' @title Update Error Description In Trajectory Tables
#' 
#' @description This is an internal utility function to update the description
#'  of errors in trajectory tables detected by the automated error detections 
#'  and correction functions of the package. 
#' 
#' @param error A character vector of error descriptions.
#' 
#' @param update A character string of the same length as \code{error} of the 
#'  error descriptions to be appended to the current error descriptions. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
.updateError <- function(error, update) {
  idxOK <- error == "OK"
  error[idxOK] <- update[idxOK]
  error[!idxOK] <- paste(error[!idxOK], update[!idxOK], sep = "+")
  error
}


#' @title Computes The perimeter Of A Polygon In Cartesian Space
#' 
#' @description Given a set of cartesian coordinates representing a polygon, 
#'  this function computes the perimeter of the polygon
#' 
#' @param x A vector of x coordinates. 
#' 
#' @param y A vector of y coordinates.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
.cartesianPerimeter <- function(x, y) {
  l <- length(x)
  d <- sqrt((x[1:(l - 1)] - x[2:l]) ^ 2 + (y[1:(l - 1)] - y[2:l]) ^ 2)
  sum(d)
}


#' @title Computes Confidence Ellipse Of A Bivariate Set Of Points
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
#' @examples
#' # TODO
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


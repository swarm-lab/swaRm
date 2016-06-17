#' @title Compute the mode(s) of a discrete distribution
#' 
#' @description 
#' 
#' @param x A vector or matrix of discrete values. 
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


#' @title Check validity of trajectory table 
#' 
#' @description Test whether a variable contains a trajectory table as produced 
#'  by the \code{\link{makeTraj}} function.
#' 
#' @param traj A variable to test. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
isTraj <- function(traj) {
  any(class(traj) == "trackTable") 
}


#' @title Check if trajectory table is using geographic coordinates
#' 
#' @description Trajectory tables produced by the \code{\link{makeTraj}} 
#'  function can use a cartesian (x, y) or a geographic (latitude, longitude) 
#'  coordinate system. This function helps determine which is being used in a 
#'  particular table. 
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
isGeo <- function(traj) {
  traj$getGeo() 
}


#' @title Update error description in trajectory tables
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


#' @title Computes the perimeter of a polygon in cartesian space
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


#' @title Computes confidence ellipse of a bivariate set of points
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
.ellipse <- memoise::memoise(function(x, y, level = 0.95) {
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
  
  qfval <- qf(level, 2, n - 1)
  
  if (eigidx[1] == 1) {
    a = sqrt(2 * eigval[2] * qfval)
    b = sqrt(2 * eigval[1] * qfval)
  } else {
    a = sqrt(2 * eigval[1] * qfval);
    b = sqrt(2 * eigval[2] * qfval);
  }
  
  alpha <- atan(eigvec[2, 1] / eigvec[2, 2])
  
  list(xC = tmp$center[1], yC = tmp$center[2], alpha = alpha, a = a, b = b)
})


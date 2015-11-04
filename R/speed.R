#' @title Linear distances along a trajectory
#' 
#' @description Given a set of cartesian coordinates representing an object's 
#'  trajectory, this function computes the linear distances between each pair of 
#'  successive locations along the trajectory.
#'  
#' @param x A vector of x (or longitude) coordinates corresponding to a single 
#'  animal trajectory. 
#' 
#' @param y A vector of y (or latitude) coordinates corresponding to a single 
#'  animal trajectory.
#'  
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'  
#' @return A vector of the same length as x and y corresponding to the linear 
#'  distances between each pair of successive locations along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{linSpeed}}, \code{\link{linAcc}}
#' 
#' @examples
#' # TODO
#' 
#' @export
linDist <- function(x, y, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || length(x) != length(y)) {
    stop("x and y must be vectors of identical length.")
  }
  
  if (geo) {
    l <- length(x)
    m1 <- cbind(x[1:(l - 1)], y[1:(l - 1)])
    m2 <- cbind(x[2:l], y[2:l])
    c(0, geosphere::distGeo(m1, m2))  
  } else {
    c(0, sqrt(diff(x) ^ 2 + diff(y) ^ 2))
  }
}


#' @title Linear speeds along a trajectory
#' 
#' @description Given a set of cartesian coordinates representing an object's 
#'  trajectory, this function computes the linear speeds between each pair of 
#'  successive locations along the trajectory.
#'  
#' @param x A vector of x (or longitude) coordinates corresponding to a single 
#'  animal trajectory. 
#' 
#' @param y A vector of y (or latitude) coordinates corresponding to a single 
#'  animal trajectory.
#'  
#' @param t A vector of timestamps corresponding to a single animal trajectory.
#'  
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'  
#' @return A vector of the same length as x and y corresponding to the linear 
#'  speeds between each pair of successive locations along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{linSpeed}}, \code{\link{linAcc}}
#' 
#' @examples
#' # TODO
#' 
#' @export
linSpeed <- function(x, y, t, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || !lubridate::is.POSIXct(t) || 
      !all.equal(length(x), length(y), length(t))) {
    stop("x, y and t must be vectors of identical length.")
  }
  
  dt <- diff(t)
  dp <- linDist(x, y, geo = geo)
  c(NA, dp[2:length(dp)] / as.numeric(dt, units = "secs"))
}


#' @title Linear accelerations along a trajectory
#' 
#' @description Given a set of cartesian coordinates representing an object's 
#'  trajectory, this function computes the linear accelerations between each pair 
#'  of successive locations along the trajectory.
#'  
#' @param x A vector of x (or longitude) coordinates corresponding to a single 
#'  animal trajectory. 
#' 
#' @param y A vector of y (or latitude) coordinates corresponding to a single 
#'  animal trajectory.
#'  
#' @param t A vector of timestamps corresponding to a single animal trajectory.
#'  
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'  
#' @return A vector of the same length as x and y corresponding to the linear 
#'  accelerations between each pair of successive locations along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{linSpeed}}, \code{\link{linAcc}}
#' 
#' @examples
#' # TODO
#' 
#' @export
linAcc <- function(x, y, t, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || !lubridate::is.POSIXct(t) || 
      !all.equal(length(x), length(y), length(t))) {
    stop("x, y and t must be vectors of identical length.")
  }
  
  s <- linSpeed(x, y, t, geo = geo)
  c(NA, diff(s))
}

#' @title Linear Distances
#' 
#' @description Given a set of locations defining a trajectory, this function 
#'  computes the linear distances between each pair of successive locations 
#'  along the trajectory.
#'  
#' @param x A vector of x (or longitude) coordinates corresponding to a single 
#'  trajectory. 
#' 
#' @param y A vector of y (or latitude) coordinates corresponding to a single 
#'  trajectory.
#'  
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'  
#' @return A vector of the same length as x and y corresponding to the linear 
#'  distances between each pair of successive locations along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{linear_speed}}, \code{\link{linear_acc}}, 
#'  \code{\link{nsd}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' linear_dist(x, y)
#' 
#' @export
linear_dist <- function(x, y, geo = FALSE) {
  if (length(x) != length(y)) 
    stop("x and y should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  if (geo) {
    l <- length(x)
    m1 <- cbind(x[1:(l - 1)], y[1:(l - 1)])
    m2 <- cbind(x[2:l], y[2:l])
    c(0, geosphere::distGeo(m1, m2))  
  } else {
    c(0, sqrt(diff(x) ^ 2 + diff(y) ^ 2))
  }
}

#' @rdname linear_dist
#' @export
linDist <- function(x, y, geo = FALSE) {
  .Deprecated("linear_dist")
  linear_dist(x, y, geo)
}


#' @title Linear Speeds
#' 
#' @description Given a set of locations defining a trajectory, this function 
#'  computes the linear speeds between each pair of successive locations along 
#'  the trajectory.
#'  
#' @param x A vector of x (or longitude) coordinates corresponding to a single 
#'  trajectory. 
#' 
#' @param y A vector of y (or latitude) coordinates corresponding to a single 
#'  trajectory.
#'  
#' @param t A vector of timestamps corresponding to a single trajectory.
#'  
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'  
#' @return A vector of the same length as x and y corresponding to the linear 
#'  speeds between each pair of successive locations along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{linear_dist}}, \code{\link{linear_acc}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' t <- as.POSIXct(1:25, origin = Sys.time())
#' linear_speed(x, y, t)
#' 
#' @export
linear_speed <- function(x, y, t, geo = FALSE) {
  if (!all(length(x) == c(length(y), length(t))))
    stop("x, y and id should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  if (!lubridate::is.POSIXct(t)) 
    stop("t should be POSIXct.")
  
  dt <- diff(t)
  dp <- linear_dist(x, y, geo = geo)
  c(NA, dp[2:length(dp)] / as.numeric(dt, units = "secs"))
}

#' @rdname linear_speed
#' @export
linSpeed <- function(x, y, t, geo = FALSE) {
  .Deprecated("linear_speed")
  linear_speed(x, y, t, geo)
}


#' @title Linear Accelerations
#' 
#' @description Given a set of locations defining a trajectory, this function 
#'  computes the linear accelerations between each pair of successive locations 
#'  along the trajectory.
#'  
#' @param x A vector of x (or longitude) coordinates corresponding to a single 
#'  trajectory. 
#' 
#' @param y A vector of y (or latitude) coordinates corresponding to a single 
#'  trajectory.
#'  
#' @param t A vector of timestamps corresponding to a single trajectory.
#'  
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'  
#' @return A vector of the same length as x and y corresponding to the linear 
#'  accelerations between each pair of successive locations along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{linear_speed}}, \code{\link{linear_dist}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' t <- as.POSIXct(1:25, origin = Sys.time())
#' linear_acc(x, y, t)
#' 
#' @export
linear_acc <- function(x, y, t, geo = FALSE) {
  if (!all(length(x) == c(length(y), length(t))))
    stop("x, y and id should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  if (!lubridate::is.POSIXct(t)) 
    stop("t should be POSIXct.")
  
  s <- linear_speed(x, y, t, geo = geo)
  c(NA, diff(s))
}

#' @rdname linear_acc
#' @export
linAcc <- function(x, y, t, geo = FALSE) {
  .Deprecated("linear_acc")
  linear_acc(x, y, t, geo)
}


#' @title Net Squared Displacement
#' 
#' @description Given a set of locations defining a trajectory, this function 
#'  computes the net squared displacement of the trajectory, that is the squared 
#'  distances between each location and the first location of the trajectory
#'  
#' @param x A vector of x (or longitude) coordinates corresponding to a single 
#'  trajectory. 
#' 
#' @param y A vector of y (or latitude) coordinates corresponding to a single 
#'  trajectory.
#'  
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#'  
#' @return A vector of the same length as x and y corresponding to the net 
#'  squared distances between each location and the first location of the 
#'  trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{linear_dist}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' nsd(x, y)
#' 
#' @export
nsd <- function(x, y, geo = FALSE) {
  if (length(x) != length(y)) 
    stop("x and y should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  if (geo) {
    m1 <- cbind(x, y)
    m2 <- matrix(c(x[1], y[1]), ncol = 2, nrow = length(x), byrow = TRUE)
    geosphere::distGeo(m1, m2) ^ 2
  } else {
    (x - x[1]) ^ 2 + (y - y[1]) ^ 2
  }
}

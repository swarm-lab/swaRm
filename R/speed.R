#' Compute instantaneous linear speed
#'
#' Given a series of spatio-temporal 2D coordinates, this function returns the 
#' instantaneous linear speed as the distance between successive positions 
#' divided by the time between these successive positions. 
#' 
#' @param x Vector of x coordinates 
#' @param y Vector of y coordinates
#' @param t Vector of time stamps
#' 
#' @return This function returns a vector with the same length as x, y and t
#' corresponding to the instantaneous linear speed along a [x, y, t] trajectory.
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @seealso \code{\link{linear_acceleration}}
#' 
#' @examples
#' # Generate and plot a random smooth trajectory
#' x <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' y <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' t <- 0:1000
#' plot(y ~ x, type = "l", asp = 1)
#' 
#' # Compute and plot linear speed
#' s <- linear_speed(x, y, t)
#' plot(s ~ t, type = "l")
#' 
#' @export
#'
linear_speed <- function(x, y, t) {
  dt <- diff(t)
  dp <- sqrt(diff(x)^2 + diff(y)^2)
  c(NA, dp / dt)
}



#' Compute instantaneous linear acceleration
#'
#' Given a series of instantaneous linear speeds as computed by 
#' \code{\link{linear_speed}}, this function returns the instantaneous linear 
#' acceleration as the difference between successive instantaneous linear speeds. 
#'
#' @param s Vector of speeds
#' 
#' @return This function returns a vector with the same length as s 
#' corresponding to the instantaneous linear acceleration at each time step.
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @seealso \code{\link{linear_speed}}
#' 
#' @examples
#' # Generate and plot a random smooth trajectory
#' x <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' y <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' t <- 0:1000
#' plot(y ~ x, type = "l", asp = 1)
#' 
#' # Compute and plot linear speed
#' s <- linear_speed(x, y, t)
#' plot(s ~ t, type = "l")
#' 
#' # Compute and plot linear acceleration
#' a <- linear_acceleration(s)
#' plot(s ~ t)
#' 
#' @export
#'
linear_acceleration <- function(s) {
  c(NA, diff(s))
}









#' Compute individual heading from a series of 2D coordinates
#'
#' Given a series of 2D coordinates, this function returns approximated headings
#' computed as the vectors between successive points. 
#' 
#' @param x Vector of x coordinates 
#' @param y Vector of y coordinates
#' 
#' @return This function returns a vector with the same length as x and y
#' corresponding to the approximated headings along a [x, y] trajectory.
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @seealso \code{\link{angular_speed}}, \code{\link{angular_acceleration}}
#' 
#' @examples
#' # Generate and plot a random smooth trajectory
#' x <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' y <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' plot(y ~ x, type = "l", asp = 1)
#' 
#' # Compute and plot distribution of headings
#' h <- heading(x, y)
#' hist(h)
#' 
#' @export
#'
heading <- function(x, y) {
  dx <- diff(x)
  dy <- diff(y)
  c(NA, atan2(dy, dx))
}



#' Compute instantaneous angular speed
#'
#' Given a series of headings as computed by \code{\link{heading}}, this 
#' function returns the angular speed as the difference between successive 
#' headings divided by the time between these successive headings. 
#' 
#' @param h Vector of headings 
#' @param t Vector of time stamps
#' 
#' @return This function returns a vector with the same length as h and t
#' corresponding to the instantaneous angular speed at each time step.
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @seealso \code{\link{heading}}, \code{\link{angular_acceleration}}
#' 
#' @examples
#' # Generate and plot a random smooth trajectory
#' x <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' y <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' t <- 0:1000
#' plot(y ~ x, type = "l", asp = 1)
#' 
#' # Compute and plot distribution of angular speeds
#' h <- heading(x, y)
#' as <- angular_speed(h, t)
#' hist(as)
#' 
#' @export
#'
angular_speed <- function(h, t) {
  dt <- diff(t)
  dh <- diff(h)
  dh[dh <= (-pi) & !is.na(dh)] <- 2 * pi + dh[dh <= (-pi) & !is.na(dh)]
  dh[dh > pi & !is.na(dh)] <- dh[dh > pi & !is.na(dh)] - 2 * pi
  c(NA, dh / dt)
}



#' Compute instantaneous angular acceleration
#'
#' Given a series of angular speeds as computed by \code{\link{angular_speed}}, 
#' this function returns the angular acceleration as the difference between 
#' successive angular speeds. 
#' 
#' @param s Vector of angular speeds
#' 
#' @return This function returns a vector with the same length as s 
#' corresponding to the instantaneous angular acceleration at each time step.
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @seealso \code{\link{heading}}, \code{\link{angular_speed}}
#' 
#' @examples
#' # Generate and plot a random smooth trajectory
#' x <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' y <- filter(cumsum(rnorm(1001)), rep(1 / 10, 10))
#' t <- 0:1000
#' plot(y ~ x, type = "l", asp = 1)
#' 
#' # Compute and plot distribution of angular accelerations
#' h <- heading(x, y)
#' as <- angular_speed(h, t)
#' aa <- angular_acceleration(as)
#' hist(aa)
#' 
#' @export
#'
angular_acceleration <- function(s) {
  c(NA, diff(s))
}
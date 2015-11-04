#' @title Estimate headings from a succession of coordinates
#'
#' @description Given a set of cartesian coordinates representing an object's 
#'  trajectory, this function returns approximated headings computed as the 
#'  direction of vectors between successive points. 
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
#' @return A vector of the same length as x and y corresponding to approximated 
#'  headings along a [x, y] trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{angSpeed}}, \code{\link{angAcc}}
#' 
#' @examples
#' # TODO
#' 
#' @export
heading <- function(x, y, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || length(x) != length(y)) {
    stop("x and y must be vectors of identical length.")
  }
  
  if (geo) {
    l <- length(x)
    m1 <- cbind(x[1:(l - 1)], y[1:(l - 1)])
    m2 <- cbind(x[2:l], y[2:l])
    c(0, geosphere::bearing(m1, m2) * pi / 180) 
  } else {
    dx <- diff(x)
    dy <- diff(y)
    c(NA, atan2(dy, dx))
  }
}


#' @title Estimate instantaneous angular speed from a succession of coordinates
#'
#' @description Given a set of cartesian coordinates representing an object's 
#'  trajectory, this function returns approximated instantaneous angular speed
#'  computed as the difference between successive headings divided by the time 
#'  between these successive headings.
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
#' @return A vector of the same length as x, y and t corresponding to approximated 
#'  instantaneous angular speed along a [x, y] trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{heading}}, \code{\link{angAcc}}
#' 
#' @examples
#' # TODO
#' 
#' @export
angSpeed <- function(x, y, t, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || !lubridate::is.POSIXct(t) || 
      !all.equal(length(x), length(y), length(t))) {
    stop("x, y and t must be vectors of identical length.")
  }
  
  dt <- diff(t)
  h <- heading(x, y, geo = geo)
  dh <- diff(h)
  dh[dh <= (-pi) & !is.na(dh)] <- 2 * pi + dh[dh <= (-pi) & !is.na(dh)]
  dh[dh > pi & !is.na(dh)] <- dh[dh > pi & !is.na(dh)] - 2 * pi
  c(NA, dh / as.numeric(dt, units = "secs"))
}


#' @title Estimate instantaneous angular acceleration from a succession of coordinates
#'
#' @description Given a set of cartesian coordinates representing an object's 
#'  trajectory, this function returns approximated instantaneous angular acceleration
#'  computed as the difference between successive angular speeds.
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
#' @return A vector of the same length as x, y and t corresponding to approximated 
#'  instantaneous angular acceleration along a [x, y] trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{heading}}, \code{\link{angAcc}}
#' 
#' @examples
#' # TODO
#' 
#' @export
angAcc <- function(x, y, t, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || !lubridate::is.POSIXct(t) || 
      !all.equal(length(x), length(y), length(t))) {
    stop("x and y must be vector of identical length.")
  }
  
  s <- angSpeed(x, y, t, geo = geo)
  c(NA, diff(s))
}

#' @title Headings
#'
#' @description Given a set of locations defining a trajectory, this function 
#'  approximates their instantaneous headings computed as the direction of the 
#'  vectors between successive locations along the trajectory. 
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
#' @return A vector of the same length as x and y corresponding to the 
#'  approximated headings along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{ang_speed}}, \code{\link{ang_acc}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' heading(x, y)
#' 
#' @export
heading <- function(x, y, geo = FALSE) {
  if (length(x) != length(y)) 
    stop("x and y should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
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


#' @title Angular Speeds
#'
#' @description Given a set of locations defining a trajectory, this function 
#'  approximates their instantaneous instantaneous angular speeds computed as 
#'  the difference between successive headings divided by the time between these 
#'  successive headings.
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
#' @return A vector of the same length as x, y and t corresponding to the 
#'  approximated instantaneous angular speeds along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{heading}}, \code{\link{ang_acc}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' t <- as.POSIXct(1:25, origin = Sys.time())
#' ang_speed(x, y, t)
#' 
#' @export
ang_speed <- function(x, y, t, geo = FALSE) {
  if (!all(length(x) == c(length(y), length(t))))
    stop("x, y and id should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  if (!lubridate::is.POSIXct(t)) 
    stop("t should be POSIXct.")
  
  dt <- diff(t)
  h <- heading(x, y, geo = geo)
  dh <- diff(h)
  dh[dh <= (-pi) & !is.na(dh)] <- 2 * pi + dh[dh <= (-pi) & !is.na(dh)]
  dh[dh > pi & !is.na(dh)] <- dh[dh > pi & !is.na(dh)] - 2 * pi
  c(NA, dh / as.numeric(dt, units = "secs"))
}

#' @rdname ang_speed
#' @export
angSpeed <- function(x, y, t, geo = FALSE) {
  .Deprecated("ang_speed")
  ang_speed(x, y, t, geo)
}


#' @title Angular Acceleration
#'
#' @description Given a set of locations defining a trajectory, this function 
#'  approximates their instantaneous angular accelerations computed as the 
#'  difference between successive angular speeds.
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
#' @return A vector of the same length as x, y and t corresponding to the
#'  approximated instantaneous angular accelerations along the trajectory.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{heading}}, \code{\link{ang_speed}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' t <- as.POSIXct(1:25, origin = Sys.time())
#' ang_acc(x, y, t)
#' 
#' @export
ang_acc <- function(x, y, t, geo = FALSE) {
  if (!all(length(x) == c(length(y), length(t))))
    stop("x, y and id should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  if (!lubridate::is.POSIXct(t)) 
    stop("t should be POSIXct.")
  
  s <- ang_speed(x, y, t, geo = geo)
  c(NA, diff(s))
}

#' @rdname ang_acc
#' @export
angAcc <- function(x, y, t, geo = FALSE) {
  .Deprecated("ang_acc")
  ang_acc(x, y, t, geo)
}

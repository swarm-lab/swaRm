#' Compute instantaneous linear speed
#'
#' Given a series of spatio-temporal 2D coordinates, this function returns the 
#' instantaneous linear speed as the distance between successive positions 
#' divided by the time between these successive positions. 
#'
linear_speed <- function(x, y, t) {
  dt <- diff(t)
  dp <- sqrt(diff(x)^2 + diff(y)^2)
  c(NA, dp / dt)
}

#' Compute instantaneous linear acceleration
#'
#' Given a series of instantaneous linear speeds as computed by 
#' \link[swarmR]{linear_speed}, this function returns the instantaneous linear 
#' acceleration as the difference between successive instantaneous linear speeds. 
#'
linear_acceleration <- function(s) {
  c(NA, diff(s))
}
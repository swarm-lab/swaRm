#' Compute distance of individuals to the center of mass of the group
#'
#' Given the 2D coordinates of all the individuals in a group, this function 
#' returns the individual distance to the center of mass (i.e. the average 
#' location) of the group.
#' 
#' @param x Vector of x coordinates. Each value must represent a different individual. 
#' @param y Vector of y coordinates. Each value must represent a different individual.
#' 
#' @return This function returns a vector with the same length as x and y
#' corresponding to the individual distances to the group's center of mass.
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#'  
#' @examples
#' # Create and plot random [x, y] coordinates
#' x <- rnorm(25)
#' y <- rnorm(25)
#' plot(y ~ x, asp = 1)
#' 
#' # Compute and plot center of mass
#' bx <- mean(x)
#' by <- mean(y)
#' points(by ~ bx, col = "red", pch = 19)
#' 
#' # Draw line between each point and the center of mass
#' mapply(
#'  function(x1, y1, x2, y2) {
#'    lines(c(y1, y2) ~ c(x1, x2), col = "red")
#'    }, 
#'  x, y, 
#'  MoreArgs = list(x2 = bx, y2 = by)
#' )
#' 
#' # Compute distance to center of mass for each individual point and plot distribution
#' d <- distance_to_barycenter(x, y)
#' hist(d)
#' 
#' @export
#'
distance_to_barycenter <- function(x, y) {
  sqrt((x - mean(x, na.rm = TRUE))^2 + (y - mean(y, na.rm = TRUE))^2)
}

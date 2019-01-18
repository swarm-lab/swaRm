#' @title Polarization order parameter
#'
#' @description Given the headings of objects in a group, this function returns 
#' the polarization order of the group. 
#' 
#' @param h A vector of headings (in radians).
#' 
#' @return A single value between 0 and 1 corresponding to the polarization 
#' order parameter of the group. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{rotOrder}}
#' 
#' @examples
#' # TODO
#' 
#' @export
polOrder <- function(h) {
  if (!is.numeric(h))
    stop("h should be numeric.")
  
  if (sum(!is.na(h)) > 1) {
    u <- matrix(c(cos(h), sin(h)), ncol = 2)
    s <- apply(u, 2, sum, na.rm = TRUE)
    sqrt(sum(s^2)) / sum(!is.na(h))
  } else {
    0
  }
}


#' @title Rotational order parameter
#'
#' @description Given the headings and cartesian coordinates of objects in a 
#'  group, this function returns the rotational order of the group. 
#' 
#' @param x A vector of x (or longitude) coordinates. 
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @param h A vector of headings (in radians).
#' 
#' @return A single value between 0 and 1 corresponding to the rotational 
#' order parameter of the group. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{polOrder}}
#' 
#' @examples
#' # TODO
#' 
#' @export
rotOrder <- function(h, x, y) {
  if (!all(length(h) == c(length(x), length(y))))
    stop("x, y and h should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y) | !is.numeric(h))
    stop("x, y and h should be numeric.")
  
  if (sum(!is.na(h)) > 1) {
    u <- matrix(c(cos(h), sin(h)), ncol = 2)
    r <- matrix(c(mean(x, na.rm = TRUE) - x, mean(y, na.rm = TRUE) - y), ncol = 2)
    r <- r / sqrt(r[,1]^2 + r[,2]^2)
    s <- apply(u * r, 2, sum, na.rm = TRUE)
    sqrt(s[1]^2 + s[2]^2) / sum(!is.na(h))
  } else {
    0
  }
}


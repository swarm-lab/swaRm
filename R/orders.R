#' @title Polarization Order Parameter
#'
#' @description Given a set of headings, this function returns the polarization 
#'  order of the set.  
#' 
#' @param h A vector of headings (in radians).
#' 
#' @return A single value between 0 and 1 corresponding to the polarization 
#'  order parameter of the group. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{rot_order}}
#' 
#' @examples
#' h <- runif(25, 0, 2 * pi)
#' pol_order(h)
#' 
#' @export
pol_order <- function(h) {
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

#' @rdname pol_order
#' @export
polOrder <- function(h) {
  .Deprecated("pol_order")
  pol_order(h)
}


#' @title Rotational Order Parameter
#'
#' @description Given a set of headings and locations, this function returns the 
#'  rotational order of the set 
#' 
#' @param x A vector of x (or longitude) coordinates. 
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @param h A vector of headings (in radians).
#' 
#' @return A single value between 0 and 1 corresponding to the rotational 
#'  order parameter of the group. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{pol_order}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' h <- runif(25, 0, 2 * pi)
#' rot_order(x, y, h)
#' 
#' @export
rot_order <- function(x, y, h) {
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

#' @rdname rot_order
#' @export
rotOrder <- function(h, x, y) {
  .Deprecated("rot_order")
  rot_order(x, y, h)
}
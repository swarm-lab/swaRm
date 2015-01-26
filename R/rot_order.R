#' Compute rotational order parameter
#'
#' Given the headings and positions of individuals in a group, this function 
#' returns the rotational order of the group. 
#' 
#' @param h Vector of headings
#' @param x Vector of x coordinates 
#' @param y Vector of y coordinates
#' 
#' @return This function returns a single value between 0 and 1 corresponding to
#' the rotational order parameter of the group. 
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @seealso \code{\link{pol_order}}
#' 
#' @examples
#' # Compute and plot polarization order parameter for groups of 100 individuals
#' # with headings varying from all equals (noise = 0) to random (noise = pi)
#' noise <- seq(0, pi, pi/64)
#' 
#' order <- sapply(noise, 
#'                 function(n) {
#'                   pol_order(runif(100, min = pi - n, max = pi + n))
#'                 }
#' )
#' 
#' plot(order ~ noise, ylim = c(0, 1))
#' 
#' @export
#'
rot_order <- function(h, x, y) {
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


#' Compute polarization order parameter
#'
#' Given the headings of individuals in a group, this function returns the 
#' polarization order of the group. 
#' 
#' @param h Vector of headings
#' 
#' @return This function returns a single value between 0 and 1 corresponding to
#' the polarization order parameter of the group. 
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @seealso \code{\link{rot_order}}
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
pol_order <- function(h) {
  if (sum(!is.na(h)) > 1) {
    u <- matrix(c(cos(h), sin(h)), ncol = 2)
    s <- apply(u, 2, sum, na.rm = TRUE)
    sqrt(sum(s^2)) / sum(!is.na(h))
  } else {
    0
  }
}
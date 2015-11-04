#' @title Compute the mode(s) of a discrete distribution
#' 
#' @description 
#' 
#' @param x A vector or matrix of discrete values. 
#' 
#' @return A vector of values corresponding to the mode(s) of x.  
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
Mode <- function(x) {
  ux <- unique(x)
  tab <- tabulate(match(x, ux))
  ux[tab == max(tab)]
}
#' @title Approximated sphericity of a set of points
#'
#' @description Given a set of cartesian coordinates, this function approximates 
#'  the sphericity of the set by calculating the bivariate 95% confidence 
#'  ellipse of the set. 
#' 
#' @param x A vector of x coordinates. 
#' 
#' @param y A vector of y coordinates.
#' 
#' @return A single numeric value corresponding to the ratio between the minor 
#'  and major axis of the bivariate 95% confidence ellipse of the set. A value 
#'  close to 1 indicates that the set is approximately circular; a value close 
#'  to 0 indicates that the set is strongly elongated. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
sphericity <- function(x, y) {
  tryCatch({
    ell <- .ellipse(x, y)
    ell$b / ell$a}, 
    error = function(e) as.numeric(NA))
}


#' @title Approximated stretching direction of a set of points
#'
#' @description Given a set of cartesian coordinates, this function approximates 
#'  the stretching direction of the set by calculating the angle of the main 
#'  axis of the bivariate 95% confidence ellipse of the set. 
#' 
#' @param x A vector of x coordinates. 
#' 
#' @param y A vector of y coordinates.
#' 
#' @return A single numeric value corresponding to the angle (in radians) of the 
#'  main axis of the bivariate 95% confidence ellipse of the set. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#'
#' @export
stretch <- function(x, y) {
  tryCatch({
    ell <- .ellipse(x, y)
    ell$alpha}, 
    error = function(e) as.numeric(NA))
}

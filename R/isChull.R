#' @title Is a point part of the convex hull of a set of points?
#'
#' @description Given a set of cartesian coordinates representing, this function 
#'  determines which of these points belongs to the convex hull (or envelope) of 
#'  the set. 
#' 
#' @param x A vector of x (or longitude) coordinates. 
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @return A logical vector of the same length as x and y. TRUE indicates that 
#' the corresponding point is part of the convex hull of the set. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
isChull <- function(x, y) {
  if ((length(x) != length(y)) | !is.numeric(x) | !is.numeric(y)) {
    stop("x and y should be numeric vectors of the same length.")
  }
  
  if (all(is.na(x)) | all(is.na(y))) {
    stop("All values in x and/or y are NAs.")
  }
  
  idx <- is.na(x) | is.na(y)
  x[idx] <- mean(x, na.rm = TRUE)
  y[idx] <- mean(y, na.rm = TRUE)
  ch <- chull(x, y)
  int <- intersect(1:length(x), ch)
  is.chull <- rep(FALSE, length(x))
  is.chull[int] <- TRUE
  is.chull
}

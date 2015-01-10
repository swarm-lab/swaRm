#' Is point part of the convex hull of a set of points?
#'
#' Given a set of 2D coordinates, this function return whether each points 
#' belongs to the convex hull (or envelope) of the set. 
#' 
#' @param x Vector of x coordinates 
#' @param y Vector of y coordinates
#' 
#' @return This function returns a logical vector with the same length as x and 
#' y. TRUE indicates that the corresponding point is part of the convex hull of 
#' the set. 
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @seealso \code{\link[grDevices]{linear_acceleration}}
#' 
#' @examples
#' # Create and plot random [x, y] coordinates
#' x <- rnorm(25)
#' y <- rnorm(25)
#' plot(y ~ x, asp = 1)
#' 
#' # Compute which point is part of the convex hull
#' is_ch <- is_chull(x, y)
#' ch <- chull(x, y) 
#' 
#' # Plot convex hull
#' points(y[is_ch] ~ x[is_ch], col = "red", pch = 19)
#' lines(c(y[ch], y[ch[1]]) ~ c(x[ch], x[ch[1]]), col = "red")
#' 
#' @export
#'
is_chull <- function(x, y) {
  # Checks
  if ((length(x) != length(y)) | !is.numeric(x) | !is.numeric(y)) {
    stop("x and y should be numeric vectors of the same length.")
  }
  if (all(is.na(x)) | all(is.na(y))) {
    stop("All values in x and/or y are NAs.")
  }
  
  # Logic
  idx <- is.na(x) | is.na(y)
  x[idx] <- mean(x, na.rm = TRUE)
  y[idx] <- mean(y, na.rm = TRUE)
  ch <- chull(x, y)
  int <- intersect(1:length(x), ch)
  is.chull <- rep(FALSE, length(x))
  is.chull[int] <- TRUE
  is.chull
}
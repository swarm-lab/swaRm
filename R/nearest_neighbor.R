#' Find nearest neihgbor
#'
#' Find the nearest neighbor of each point in a set of 2D points, and compute
#' the nearest neighbor distances.
#' 
#' @param x Vector of x coordinates 
#' @param y Vector of y coordinates
#' 
#' @return This function returns a data frame with 3 columns and with the same 
#' number of rows as the lengths of x and y. Each row corresponds to an 
#' individual. Column \code{id} corresponds to the identity of each individual. 
#' Column \code{nn} corresponds to the identity of the nearest neighbor of each 
#' individual. Column \code{nnd} corresponds to the distance between each 
#' individual and its nearest neighbor. 
#' 
#' @author Simon Garnier: \email{garnier@@njit.edu}, \link[https://twitter.com/sjmgarnier]{@@sjmgarnier}
#' 
#' @examples
#' # Generate and plot random points
#' x <- rnorm(25)
#' y <- rnorm(25)
#' plot(y ~ x, asp = 1)
#' 
#' # Compute nearest neighbors
#' neighbors <- nearest_neighbor(x, y)
#' 
#' # Plot nearest neighbor network (arrow points from the focal individual to its 
#' # nearest neighbor)
#' apply(neighbors, 1, 
#'       function(n, x, y) {
#'         arrows(x[n[1]], y[n[1]], x[n[2]], y[n[2]], length = 0.1, col = "red")
#'       },
#'       x = x, y = y
#' )
#' 
#' # Plot distribution of nearest neighbor distances
#' hist(neighbors$nnd)
#' 
#' @export
#'
nearest_neighbor <- function(x, y) {
  # Checks
  if ((length(x) != length(y)) | !is.numeric(x) | !is.numeric(y)) {
    stop("x and y should be numeric vectors of the same length.")
  }
  if (all(is.na(x)) | all(is.na(y))) {
    stop("All values in x and/or y are NAs.")
  }
  
  # Logic
  d <- as.matrix(dist(cbind(x, y)))
  diag(d) <- NA
  d[is.na(x) | is.na(y), ] <- NA
  d[, is.na(x) | is.na(y)] <- NA
  nn <- apply(d, 2, 
              function(x) {
                if (sum(is.na(x)) != length(x)) {
                  which(x == min(x, na.rm = TRUE))[1]
                } else {
                  NA
                }
              })
  nnd <- apply(d, 2, 
               function(x) {
                 if (sum(is.na(x)) != length(x)) {
                   min(x, na.rm = TRUE)
                 } else {
                   NA
                 }
               })
  data.frame(id = 1:length(x), nn = nn, nnd = nnd)
}

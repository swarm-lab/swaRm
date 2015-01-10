nearest_neighbor <- function(x, y) {
  # Find the nearest neighbor of each point in a set of 2D points, and compute
  # the nearest neighbor distances. 
  #
  # Args:
  #   x: Vector of x locations.
  #   y: Vector of y locations.
  #
  # Returns:
  #   A logical vector of the same length as x and y. TRUE indicates that the 
  #   corresponding point is part of the convex hull of the set. 
  
  # Checks
  if ((length(x) != length(y)) | !is.numeric(x) | !is.numeric(y)) {
    stop("x and y should be numeric vectors of the same length.")
  }
  if (all(is.na(x)) | all(y)) {
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
  list(nn = nn, nnd = nnd)
}

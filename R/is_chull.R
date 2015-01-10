is_chull <- function(x, y) {
  # Find which points form the convex hull of a set of 2D points. 
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
  idx <- is.na(x) | is.na(y)
  x[idx] <- mean(x, na.rm = TRUE)
  y[idx] <- mean(y, na.rm = TRUE)
  ch <- chull(x, y)
  int <- intersect(1:length(x), ch)
  is.chull <- rep(FALSE, length(x))
  is.chull[int] <- TRUE
  is.chull
}
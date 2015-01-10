distance_to_barycenter <- function(x, y) {
  sqrt((x - mean(x, na.rm = TRUE))^2 + (y - mean(y, na.rm = TRUE))^2)
}

pol_order <- function(angle) {
  if (sum(!is.na(angle)) > 1) {
    u <- matrix(c(cos(angle), sin(angle)), ncol = 2)
    s <- apply(u, 2, sum, na.rm = TRUE)
    sqrt(sum(s^2)) / sum(!is.na(angle))
  } else {
    0
  }
}
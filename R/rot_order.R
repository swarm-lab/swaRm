rot_order <- function(angle, x, y) {
  if (sum(!is.na(angle)) > 1) {
    u <- matrix(c(cos(angle), sin(angle)), ncol = 2)
    r <- matrix(c(mean(x, na.rm = TRUE) - x, mean(y, na.rm = TRUE) - y), ncol = 2)
    s <- apply(u * r, 2, sum, na.rm = TRUE)
    sqrt(s[1]^2 + s[2]^2) / sum(!is.na(angle))
  } else {
    0
  }
}


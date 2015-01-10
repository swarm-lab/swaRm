abs_heading <- function(x, y) {
  dx <- diff(x)
  dy <- diff(y)
  c(NA, atan2(dy, dx))
}

rel_heading <- function(abs.heading) {
  dh <- diff(abs.heading)
  dh[dh <= (-pi) & !is.na(dh)] <- 2 * pi + dh[dh <= (-pi) & !is.na(dh)]
  dh[dh > pi & !is.na(dh)] <- dh[dh > pi & !is.na(dh)] - 2 * pi
  c(NA, dh)
}
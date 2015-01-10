# Functions for individual trajectories
MakeTraj <- function(x, y, time = NULL, fps = NULL, all = TRUE, 
                     abs.heading = all, rel.heading = all,
                     convex.hull = all, nearest.neighbor = all,
                     speed = all, acceleration = all,
                     distance.to.barycenter = all) {
  # Description.
  #
  # Args:
  #   arg: Argument.
  #
  # Returns:
  #   Returns. 
  
  require("data.table")   # To be removed once packaged
  require("dplyr")        # To be removed once packaged
  
  # Checks
  if (!is.matrix(x) | !is.matrix(y) | !all.equal(dim(x), dim(y))) {
    stop("x and y must be matrices of identical dimensions. Each column of matrices x and y should represent the successive x and y (respectively) spatial coordinates of a single individual. Each line should represent a different time step.")
  }
  if (length(time) > 0 & nrow(x) != length(time)) {
    stop("time should be a vector of length nrow(x).")
  }
  if (length(time) > 0) {   # Not clean but prevent a warning message from sort when time = NULL
    if (!all(time == sort(time))) {
      stop("time should contain increasing values only.")
    }
  }
  
  # Build basic data table
  n.individuals <- ncol(x)
  n.steps <- nrow(x)
  traj <- data.table(id = as.factor(rep(1:n.individuals, 
                                        each = n.steps)),
                     t = NA,  # Placeholder, time will be set later in the function
                     x = as.vector(x),
                     y = as.vector(y))
  
  # Add time
  traj <- mutate(traj, test = 0)
    
#   traj <- mutate(traj, t = if (length(time) > 0) rep(time, n.individuals)
#                  else if (length(fps) > 0) rep(0:(n.steps - 1) / fps, n.individuals)
#                  else rep(0:(n.steps - 1), n.individuals))
  
  # Compute additional statistics
  if (is.matrix(abs.heading)) {
    mutate(traj, abs.heading = as.vector(abs.heading))
  } else if (abs.heading | rel.heading) {
    traj <- traj %>%
      group_by(id) %>%
      mutate(abs.heading = ComputeAbsHeading(x, y)) %>%
      ungroup()
  }
  
  traj <- traj %>%
    group_by(id) %>%
    mutate(rel.heading = if(rel.heading) ComputeRelHeading(abs.heading) else NA,
           speed = if(speed | acceleration) ComputeSpeed(x, y) else NA,
           acceleration = if(acceleration) ComputeAcceleration(speed) else NA) %>%
    ungroup() %>%
    group_by(t) %>%
    mutate(convex.hull = if(convex.hull) IsChull(x, y) else NA,
           nearest.neighbor.id = if(nearest.neighbor) ComputeNearestNeighbor(x, y)[[1]] else NA,
           nearest.neighbor.distance = if(nearest.neighbor) ComputeNearestNeighbor(x, y)[[2]] else NA,
           distance.to.barycenter = if(distance.to.barycenter) ComputeDistanceToBarycenter(x, y) else NA) %>%
    ungroup()
}

IsChull <- function(x, y) {
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

ComputeNearestNeighbor <- function(x, y) {
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

ComputeAbsHeading <- function(x, y) {
  dx <- diff(x)
  dy <- diff(y)
  c(NA, atan2(dy, dx))
}

ComputeRelHeading <- function(abs.heading) {
  dh <- diff(abs.heading)
  dh[dh <= (-pi) & !is.na(dh)] <- 2 * pi + dh[dh <= (-pi) & !is.na(dh)]
  dh[dh > pi & !is.na(dh)] <- dh[dh > pi & !is.na(dh)] - 2 * pi
  c(NA, dh)
}

ComputeSpeed <- function(x, y) {
  c(NA, sqrt(diff(x)^2 + diff(y)^2))
}

ComputeAcceleration <- function(speed) {
  c(NA, diff(speed))
}

ComputeDistanceToBarycenter <- function(x, y) {
  sqrt((x - mean(x, na.rm = TRUE))^2 + (y - mean(y, na.rm = TRUE))^2)
}

# Functions for group statistics
ComputeGroupStats <- function(TRAJ, all = TRUE,
                              nearest.neighbor = all, convex.hull = all,
                              polarization.order = all, rotation.order = all,
                              barycenter = all) {
  # Description.
  #
  # Args:
  #   arg: Argument.
  #
  # Returns:
  #   Returns. 
  
  require("data.table")   # To be removed once packaged
  require("dplyr")        # To be removed once packaged
  require("splancs")
  
  # Checks
  if (nearest.neighbor & !any(names(traj) == "nearest.neighbor.distance")) {
    stop("Nearest neighbor distances missing from traj table. Please recompute traj by setting nearest.neighbor to TRUE.")
  }
  if (convex.hull & !any(names(traj) == "convex.hull")) {
    stop("Convex hull envelope missing from traj table. Please recompute traj by setting convex.hull to TRUE.")
  }
  if ((polarization.order | rotation.order) & !any(names(traj) == "abs.heading")) {
    stop("Absolute heading missing from traj table. Please recompute traj by setting abs.heading to TRUE.")
  }
  
  # Compute group statistics
  traj %>%
    group_by(t) %>%
    summarise(
      mean.nearest.neighbor.distance = if (nearest.neighbor) mean(nearest.neighbor.distance, na.rm = TRUE) else NA,
      convex.hull.area = if (nearest.neighbor) areapl(cbind(x[convex.hull], y[convex.hull])) else NA,
      bary.x = if (barycenter) mean(x, na.rm = TRUE) else NA,
      bary.y = if (barycenter) mean(y, na.rm = TRUE) else NA,
      distance.to.barycenter = if (barycenter) mean(distance.to.barycenter, na.rm = TRUE) else NA,
      polarization.order = if (polarization.order) ComputePolOrder(abs.heading) else NA,
      rotation.order = if (rotation.order) ComputeRotOrder(abs.heading) else NA)
}

ComputePolOrder <- function(angle) {
  if (sum(!is.na(angle)) > 1) {
    u <- matrix(c(cos(angle), sin(angle)), ncol = 2)
    s <- apply(u, 2, sum, na.rm = TRUE)
    sqrt(sum(s^2)) / sum(!is.na(angle))
  } else {
    0
  }
}

ComputeRotOrder <- function(angle, x, y) {
  if (sum(!is.na(angle)) > 1) {
    u <- matrix(c(cos(angle), sin(angle)), ncol = 2)
    r <- matrix(c(mean(x, na.rm = TRUE) - x, mean(y, na.rm = TRUE) - y), ncol = 2)
    s <- apply(u * r, 2, sum, na.rm = TRUE)
    sqrt(s[1]^2 + s[2]^2) / sum(!is.na(angle))
  } else {
    0
  }
}



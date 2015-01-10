make_traj <- function(x, y, time = NULL, fps = NULL, all = TRUE, 
                     abs.heading = all, rel.heading = all,
                     convex.hull = all, nearest.neighbor = all,
                     speed = all, acceleration = all,
                     distance.to.barycenter = all) {
  
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
group_summary <- function(TRAJ, all = TRUE,
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
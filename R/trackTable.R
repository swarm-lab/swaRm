#' @title Check validity of trajectory table 
#' 
#' @description Test whether a variable contains a trajectory table as produced 
#'  by the \code{\link{makeTraj}} function.
#' 
#' @param object An object to test. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
is.trackTable <- function(x) {
  any(class(x) == "trackTable") 
}


#' @title Check if trajectory table is using geographic coordinates
#' 
#' @description Trajectory tables produced by the \code{\link{makeTraj}} 
#'  function can use a cartesian (x, y) or a geographic (latitude, longitude) 
#'  coordinate system. This function helps determine which is being used in a 
#'  particular table. 
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
is.geo <- function(x) {
  ifelse(tibble::has_name(x, "x"), FALSE, TRUE) 
}


#' @title Check if trajectory table is 2D or 3D
#' 
#' @description Trajectory tables produced by the \code{\link{makeTraj}} 
#'  function can have 2 or 3 dimensions. This function helps determine which is 
#'  being used in a particular table. 
#'
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
is.2D <- function(x) {
  ifelse(any(tibble::has_name(x, c("alt", "z"))), FALSE, TRUE)
}


#' @export
print.trackTable <- function(x, ...) {
  stopifnot(is.trackTable(x))
  
  nObs <- nrow(x)
  nTracks <- length(unique(x$id))
  geo <- is.geo(x)
  type <- ifelse(is.2D(x), "2D", "3D")
  
  cat("Trajectory table [", nObs, " observations]\n", sep = "")
  cat("Number of tracks: ", nTracks, "\n")
  cat("Type: ", type, "\n")
  cat("Geographic: ", geo, "\n")
  cat("\n")
  tibble:::print.tbl_df(x, ...)
}
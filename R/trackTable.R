#' @title Track table class
#' 
#' @description A track table is a regular \code{\link[data.table:data.table-package]{data.table}}
#'  enhanced to be used by the functions in \code{\link[swaRm:swaRm-package]{swaRm}}.
#'  It is a standardized way to store geographic or projected, single or group, 
#'  2D or 3D trajectories. 
#' 
#' @param id A vector of identifiers. Each identifier should correspond to the 
#'  coordinates of a single trajectory. Identifiers can be numbers or character
#'  strings. 
#' 
#' @param t A vector of standardized date-time values. 
#' 
#' @param x A vector of x (or longitude) coordinates. 
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @param z A vector of z (or altitude) coordinates.
#' 
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (longitude/latitude/altitude values) or projected 
#'  coordinates (x/y/z values in meters). Default: FALSE. 
#' 
#' @return A trajectory table with 4 or 5 columns:
#'  \itemize{
#'    \item{"id": }{the unique identifier of the trajectory.}
#'    \item{"time": }{the full timestamp (date+time) of each location in 
#'      \code{\link{POSIXct}} format.}
#'    \item{"x" or "lon": }{the x or longitude coordinates of the trajectories.}
#'    \item{"y" or "lat": }{the y or latitude coordinates of the trajectories.}
#'    \item{"z" or "alt": }{the z or altitude coordinates of the trajectories.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}
#' 
#' @examples
#' # TODO
#' 
#' @export
trackTable <- function(id, t, x, y, z = NULL, geo = FALSE) {
  if (geo) {
    if (is.null(z)) {
      DT <- data.table::data.table(id = factor(id), time = t, lon = x, lat = y)
      class(DT) <- append("2D", class(DT))
      class(DT) <- append("geographic", class(DT))
      class(DT) <- append("trackTable", class(DT))
    } else {
      DT <- data.table::data.table(id = factor(id), time = t, lon = x, lat = y, alt = z)
      class(DT) <- append("3D", class(DT))
      class(DT) <- append("geographic", class(DT))
      class(DT) <- append("trackTable", class(DT))
    }
  } else {
    if (is.null(z)) {
      DT <- data.table::data.table(id = factor(id), time = t, x = x, y = y)
      class(DT) <- append("2D", class(DT))
      class(DT) <- append("projected", class(DT))
      class(DT) <- append("trackTable", class(DT))
    } else {
      DT <- data.table::data.table(id = factor(id), time = t, x = x, y = y, z = z)
      class(DT) <- append("3D", class(DT))
      class(DT) <- append("projected", class(DT))
      class(DT) <- append("trackTable", class(DT))
    }
  }
  
  return(DT)
}


#' @export
print.trackTable <- function(x, ..., nrows = 10L) {
  cat("Trajectory table [", nrow(x), " observations]\n", sep = "")
  cat("Number of tracks: ", length(unique(x$id)), "\n")
  cat("Geographic data: ", tolower(any(class(x) == "geographic")), "\n")
  cat("Dimensions: ", ifelse(tolower(any(class(x) == "2D")), "2D", "3D"), "\n")
  cat("\n")
  data.table:::print.data.table(x, nrows = nrows)
  
  invisible(x)
}





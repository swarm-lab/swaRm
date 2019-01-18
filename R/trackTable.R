#' @title Build Track
#' 
#' @description Given a set of coordinates representing an object's trajectory, 
#'  this function builds a trajectory table that can then be used to work with 
#'  the functions provided by the \code{\link[swaRm:swaRm-package]{swaRm}} package.
#' 
#' @param id A unique identifier for the trajectory. It can be a number or a 
#'  character string. If not set, it defaults to 0. Multiple track identifiers 
#'  can be passed to the function, in which case \code{id} should be a vector of 
#'  the same length as \code{x}.
#' 
#' @param x,y,z Vectors of coordinates. Note that \code{z} is not required. If 
#'  \code{geo=TRUE}, then \code{x} will correspond to longitudes, \code{y} to 
#'  latitudes and \code{z} to altitudes. 
#' 
#' @param t A numeric vector of the time elapsed since the beginning of the 
#'  tracking. The time units are specified in \code{units}. 
#'  
#' @param t0 The time at which tracking began. It can be any object that can be 
#'  converted to a date-time by \code{\link[lubridate]{as_datetime}}. If not 
#'  specified, it defaults to the date-time returned by \code{\link[lubridate]{now}}.
#'  
#' @param units A character string indicating the units in which \code{t} as been 
#'  recorded (default: "secs"). See \code{\link{units}} for more details. 
#'  
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (default: FALSE).
#'  
#' @param tz An optional time zone specification for \code{t0}. If not specified,
#'  it defaults to the system's time zone. See \code{\link{timezone}} for more \
#'  details. 
#'  
#' @return A trajectory table.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link[lubridate]{as_datetime}}, \code{\link[lubridate]{now}}, 
#'  \code{\link{units}}, \code{\link{timezone}}
#' 
#' @examples
#' # TODO
#'
#' @export
track <- function(id = NULL, x, y, z = NULL, t, t0 = NULL, units = "secs", 
                  geo = FALSE, tz = NULL) {
  if (!all(length(x) == c(length(y), ifelse(is.null(z), length(x), length(z)), length(t))))
    stop("x, y, z (when available) and t should all have the same length.")
  
  if (is.null(id)) {
    id <- 0
  } else {
    if (length(id) > 1 & length(id) != length(x))
      stop("id should have a length of 1 or the same length as x.")
  }
  
  if (is.null(tz))
    tz <- Sys.timezone()
  
  if (is.null(t0)) {
    t0 <- lubridate::now(tz)
  } else {
    t0 <- tryCatch(lubridate::as_datetime(t0, tz = tz), 
                   warning = function(w) {
                     stop("t0 could not be converted to a date-time (POSIXct) object.")
                   })
  }
  
  time <- t0 + as.difftime(as.numeric(t), units = units)
  
  if (geo) {
    if (is.null(z))
      tab <- tibble::tibble(id = id, time = time, lon = x, lat = y)
    else
      tab <- tibble::tibble(id = id, time = time, lon = x, lat = y, alt = z)
  } else {
    if (is.null(z))
      tab <- tibble::tibble(id = id, time = time, x = x, y = y)
    else
      tab <- tibble::tibble(id = id, time = time, x = x, y = y, z = z)
  }
  
  class(tab) <- c("trackTable", class(tab))
  tab
}


#' @title Check validity of trajectory table 
#' 
#' @description Test whether a variable contains a trajectory table as produced 
#'  by the \code{\link{makeTraj}} function.
#' 
#' @param x An object to test. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
is.trackTable <- function(x) {
  any(class(x) == "trackTable") & 
    (all(tibble::has_name(x, c("id", "time", "x", "y"))) |
       all(tibble::has_name(x, c("id", "time", "lon", "lat"))))
}


#' @title Check if trajectory table is using geographic coordinates
#' 
#' @description Trajectory tables produced by the \code{\link{makeTraj}} 
#'  function can use a cartesian (x, y) or a geographic (latitude, longitude) 
#'  coordinate system. This function helps determine which is being used in a 
#'  particular table. 
#' 
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
is.geo <- function(x) {
  if (!is.trackTable(x))
    stop("This is not a trackTable object.")
  
  !tibble::has_name(x, "x")
}


#' @title Check if trajectory table is 2D or 3D
#' 
#' @description Trajectory tables produced by the \code{\link{makeTraj}} 
#'  function can have 2 or 3 dimensions. This function helps determine which is 
#'  being used in a particular table. 
#'
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @examples
#' # TODO
#' 
#' @export
is.2D <- function(x) {
  if (!is.trackTable(x))
    stop("This is not a trackTable object.")
  
  !any(tibble::has_name(x, c("alt", "z")))
}


#' @export
print.trackTable <- function(x, ..., n = NULL, width = NULL, n_extra = NULL) {
  if (!is.trackTable(x)) {
    warning("This is a malformed trackTable. Printing as is.")
    print.AsIs(x)
  } else {
    nObs <- nrow(x)
    nTracks <- length(unique(x$id))
    geo <- is.geo(x)
    type <- ifelse(is.2D(x), "2D", "3D")
    
    cat("Trajectory table [", nObs, " observations]\n", sep = "")
    cat("Number of tracks: ", nTracks, "\n")
    cat("Type: ", type, "\n")
    cat("Geographic: ", geo, "\n")
    cat("\n")
    
    cat(paste0(format(x, ..., n = n, width = width, n_extra = n_extra), "\n"), sep = "")
    invisible(x)
  }
}
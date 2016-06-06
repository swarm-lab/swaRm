#' @title Build a trajectory table
#' 
#' @description Given a set of cartesian coordinates representing an object's trajectory, 
#'  this function builds a trajectory table that can then be used to work with 
#'  the functions provided by the \code{\link[swaRm:swaRm-package]{swaRm}}
#'  package. 
#' 
#' @param x A vector of x (or longitude) coordinates corresponding to a single 
#'  animal trajectory. 
#' 
#' @param y A vector of y (or latitude) coordinates corresponding to a single 
#'  animal trajectory.
#' 
#' @param id A unique identifier for the trajectory. It can be a number or a 
#'  character string. If not set, it defaults to zero. 
#' 
#' @param date A character string corresponding to a date in year-month-day 
#'  format. Alternatively, a vector of character strings of the same length as x
#'  and y (for instance if the trajectory spans multiple days). If not set, it 
#'  default to January 1st, 1970. 
#' 
#' @param time A vector of character strings corresponding to the timestamps of 
#'  each location of the trajectory, in hours-minutes-seconds format. If not set,
#'  the first timestamp defaults to 0 hours, 0 minutes, 1 second. The following 
#'  timestamps are either in increment of 1 second or 1 second / fps. 
#'  
#' @param date.format A character string with 3 letters ('y' for year, 'm' for 
#'  month, and 'd' for day) indicating the format of the date (e.g., 'ymd' for 
#'  year-month-day, 'dmy' for day-month-year, etc).
#'  
#' @param time.format A character string with 2 or 3 letters ('h' for hour, 'm' 
#'  for minute, and 's' for second) indicating the format of the time (e.g., 
#'  'hms' for hour-minute-second, 'hm' for hour-minute, and 'ms' for 
#'  minute-second).
#'  
#' @param tz A character string identifying the timezone of the timestamps of 
#'  the trajectory. Default: "UTC". 
#' 
#' @param fps A single numeric value corresponding to the sampling rate of the 
#'  trajectory. 
#' 
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE. 
#' 
#' @return A data table (from the \code{\link[data.table:data.table-package]{data.table}}
#'  package) with four columns:
#'  \itemize{
#'    \item{"id": }{the unique identifier of the trajectory.}
#'    \item{"x" or "lon": }{the x or longitude coordinates of the trajectory.}
#'    \item{"y" or "lat": }{the y or latitude coordinates of the trajectory.}
#'    \item{"time": }{the full timestamp (date+time) of each location in 
#'      \code{\link{POSIXct}} format.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{fixTIMEDUP}}, \code{\link{fixTIMESEQ}}, \code{\link{fixTIMENA}},
#'  \code{\link{fixLOCSEQ}}, \code{\link{fixLOCNA}}, \code{\link{fixMISSING}}
#' 
#' @examples
#' # TODO
#' 
#' @export
makeTraj <- function(x, y, id = NULL, date = NULL, time = NULL, 
                     date.format = "ymd", time.format = "hms", tz = "UTC", 
                     fps = NULL, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || length(x) != length(y)) {
    stop("x and y must be vector of identical length.")
  }
  
  if (length(time) > 0 && length(x) != length(time)) {
    stop("time should be a vector of the same length as x and y.")
  }
  
  if (geo) {
    traj <- data.table::data.table(id = factor(ifelse(is.null(id), 0, id)),
                                   lon = x, lat = y, time = NA)
  } else {
    traj <- data.table::data.table(id = factor(ifelse(is.null(id), 0, id)),
                                   x = x, y = y, time = NA)
  }
  
  if (is.null(date) && is.null(time) && is.null(fps)) {
    traj$time <- ISOdate(1970, 1, 1, tz = tz) + 1:nrow(traj)
  } else if (is.null(date) && is.null(time)) {
    traj$time <- ISOdate(1970, 1, 1, tz = tz) + 1:nrow(traj) / fps
  } else if (is.null(date)) {
    fn <- get(paste0("ymd_", time.format), asNamespace("lubridate"))
    traj$time <- do.call(fn, list(paste("1970-01-01", time), tz = tz))
  } else {
    fn <- get(paste0(date.format, "_", time.format), asNamespace("lubridate"))
    traj$time <- do.call(fn, list(paste(date, time), tz = tz))
  }
  
  traj
}







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
#' @param z A vector of z (or altitude) coordinates corresponding to a single 
#'  animal trajectory (default: NULL). 
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
#' @return A trajectory table (see \code{\link{trackTable}}).
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{trackTable}}
#' 
#' @examples
#' # TODO
#' 
#' @export
makeTraj <- function(x, y, z = NULL, id = NULL, date = NULL, time = NULL, 
                     date.format = "ymd", time.format = "hms", tz = "UTC", 
                     fps = NULL, geo = FALSE) {
  if (!is.vector(x) || !is.vector(y) || !is.vector(x) || length(x) != length(y))
    stop("x, y (and z, if applicable) must be vector of identical length.")
  
  if (!is.null(z) && ((length(z) != length(x)) || (length(z) != length(y))))
    stop("x, y (and z, if applicable) must be vector of identical length.")
  
  if (length(time) > 0 && length(x) != length(time))
    stop("time should be a vector of the same length as x and y (and z, if applicable).")
  
  id <- ifelse(is.null(id), 0, id)
  
  if (is.null(date) && is.null(time) && is.null(fps)) {
    time <- ISOdate(1970, 1, 1, tz = tz) + 1:length(x)
  } else if (is.null(date) && is.null(time)) {
    time <- ISOdate(1970, 1, 1, tz = tz) + 1:length(x) / fps
  } else if (is.null(date)) {
    fn <- get(paste0("ymd_", time.format), asNamespace("lubridate"))
    time <- do.call(fn, list(paste("1970-01-01", time), tz = tz))
  } else {
    fn <- get(paste0(date.format, "_", time.format), asNamespace("lubridate"))
    time <- do.call(fn, list(paste(date, time), tz = tz))
  }
  
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







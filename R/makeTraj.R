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


#' @title Correct duplicated timestamps in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  duplicated timestamps in trajectory tables. If it cannot correct a duplicated
#'  timestamp, it replaces it with NA. 
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @param step A \code{\link{difftime}} object representing the time between two 
#'  consecutive locations of the trajectory. If not set, it is set to the most 
#'  common time difference between successive locations in \code{traj}.
#' 
#' @return A data table (from the \code{\link[data.table:data.table-package]{data.table}}
#'  package) with five columns:
#'  \itemize{
#'    \item{"id": }{the unique identifier of the trajectory.}
#'    \item{"x" or "lon": }{the x or longitude coordinates of the trajectory.}
#'    \item{"y" or "lat": }{the y or latitude coordinates of the trajectory.}
#'    \item{"time": }{the full timestamp (date+time) of each location in 
#'      \code{\link{POSIXct}} format.}
#'    \item{"error": }{the type of error detected and corrected (when possible) 
#'      by the function.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMESEQ}}, \code{\link{fixTIMENA}},
#'  \code{\link{fixLOCSEQ}}, \code{\link{fixLOCNA}}, \code{\link{fixMISSING}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixTIMEDUP <- function(traj, step = NULL) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (is.null(traj$error)) {
    traj$error <- rep("OK", nrow(traj))
  }
  
  if (is.null(step)) {
    d <- diff(traj$time)
    u <- units(d)
    step <- as.difftime(Mode(d)[1], units = u)
  }
  
  idxDUP <- which(duplicated(traj$time) & !is.na(traj$time))
  
  traj$error[idxDUP] <- .updateError(traj$error[idxDUP], rep("timeDUP", length(idxDUP)))
  
  resolved <- !((traj$time[idxDUP - 1] + step) %in% traj$time)
  traj$time[idxDUP[resolved]] <- traj$time[idxDUP[resolved] - 1] + step
  traj$time[idxDUP[!resolved]] <- NA
  traj$error[idxDUP[!resolved]] <- "OK"
  
  traj
}


#' @title Correct inconsistent timestamps in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  inconsistent timestamps (for instance due to a writing error) in trajectory 
#'  tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#'
#' @param step A \code{\link{difftime}} object representing the time between two 
#'  consecutive locations of the trajectory. If not set, it is set to the most 
#'  common time difference between successive locations in \code{traj}.
#' 
#' @return A data table (from the \code{\link[data.table:data.table-package]{data.table}}
#'  package) with five columns:
#'  \itemize{
#'    \item{"id": }{the unique identifier of the trajectory.}
#'    \item{"x" or "lon": }{the x or longitude coordinates of the trajectory.}
#'    \item{"y" or "lat": }{the y or latitude coordinates of the trajectory.}
#'    \item{"time": }{the full timestamp (date+time) of each location in 
#'      \code{\link{POSIXct}} format.}
#'    \item{"error": }{the type of error detected and corrected (when possible) 
#'      by the function.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMEDUP}}, \code{\link{fixTIMENA}},
#'  \code{\link{fixLOCSEQ}}, \code{\link{fixLOCNA}}, \code{\link{fixMISSING}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixTIMESEQ <- function(traj, step = NULL) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (is.null(traj$error)) {
    traj$error <- rep("OK", nrow(traj))
  }
  
  if (is.null(step)) {
    d <- diff(traj$time)
    u <- units(d)
    step <- as.difftime(Mode(d)[1], units = u)
  }
  
  m <- MASS::rlm(as.numeric(traj$time) ~ c(1:length(traj$time)))
  r <- sqrt(abs(m$residuals))
  pos <- as.numeric(names(r))
  out <- (r > median(r) + 1.5 * IQR(r)) | (r < median(r) - 1.5 * IQR(r))
  idxSEQ <- pos[out]
  
  traj$error[idxSEQ] <- .updateError(traj$error[idxSEQ], rep("timeSEQ", length(idxSEQ)))
  
  for (i in 1:length(idxSEQ)) {
    if (idxSEQ[i] != 1) {
      if (!((traj$time[idxSEQ[i] - 1] + step) %in% traj$time)) {
        traj$time[idxSEQ[i]] <- traj$time[idxSEQ[i] - 1] + step
      } else {
        traj$time[idxSEQ[i]] <- NA
        traj$error[idxSEQ[i]] <- "OK"
      }
    }
  }
  
  traj
}


#' @title Correct NA timestamps in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  NA timestamps in trajectory tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @param spline If \code{spline} is \code{TRUE}, missing timestamps are estimated
#'  using spline interpolation. If \code{FALSE} (the default), a linear interpolation
#'  is used instead. 
#' 
#' @return A data table (from the \code{\link[data.table:data.table-package]{data.table}}
#'  package) with five columns:
#'  \itemize{
#'    \item{"id": }{the unique identifier of the trajectory.}
#'    \item{"x" or "lon": }{the x or longitude coordinates of the trajectory.}
#'    \item{"y" or "lat": }{the y or latitude coordinates of the trajectory.}
#'    \item{"time": }{the full timestamp (date+time) of each location in 
#'      \code{\link{POSIXct}} format.}
#'    \item{"error": }{the type of error detected and corrected (when possible) 
#'      by the function.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMEDUP}}, \code{\link{fixTIMESEQ}},
#'  \code{\link{fixLOCSEQ}}, \code{\link{fixLOCNA}}, \code{\link{fixMISSING}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixTIMENA <- function(traj, spline = FALSE) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (is.null(traj$error)) {
    traj$error <- rep("OK", nrow(traj))
  }
  
  idxNA <- which(is.na(traj$time))
  
  traj$error[idxNA] <- .updateError(traj$error[idxNA], rep("timeNA", length(idxNA)))
  
  if (spline) {
    interp <- zoo::na.spline(traj$time, na.rm = FALSE)
  } else {
    interp <- zoo::na.approx(traj$time, na.rm = FALSE) 
  }
  
  traj$time <- as.POSIXct(interp, origin = "1970-01-01", tz = lubridate::tz(traj$time))
  
  traj
}


#' @title Correct inconsistent locations in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  inconsistent locations (for instance due to a writing error) in trajectory 
#'  tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#'  
#' @param s The discrmination threshold of the outlier detection algorithm. 
#'  Higher values correspond to less outliers.  
#'
#' @param spline If \code{spline} is \code{TRUE}, inconsistent locations are 
#'  estimated using spline interpolation. If \code{FALSE} (the default), a linear 
#'  interpolation is used instead.
#' 
#' @return A data table (from the \code{\link[data.table:data.table-package]{data.table}}
#'  package) with five columns:
#'  \itemize{
#'    \item{"id": }{the unique identifier of the trajectory.}
#'    \item{"x" or "lon": }{the x or longitude coordinates of the trajectory.}
#'    \item{"y" or "lat": }{the y or latitude coordinates of the trajectory.}
#'    \item{"time": }{the full timestamp (date+time) of each location in 
#'      \code{\link{POSIXct}} format.}
#'    \item{"error": }{the type of error detected and corrected (when possible) 
#'      by the function.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMEDUP}}, \code{\link{fixTIMESEQ}}, 
#'  \code{\link{fixTIMENA}}, \code{\link{fixLOCNA}}, \code{\link{fixMISSING}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixLOCSEQ <- function(traj, s = 6, spline = FALSE) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  geo <- .isGeo(traj)
  
  if (geo) {
    m1 <- loess(lon ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- abs(residuals(m1))
    m1 <- loess(lon ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
    m2 <- loess(lat ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- abs(residuals(m2))
    m2 <- loess(lat ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
  } else {
    m1 <- loess(x ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- abs(residuals(m1))
    m1 <- loess(x ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
    m2 <- loess(y ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- abs(residuals(m2))
    m2 <- loess(y ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
  }
  
  r1 <- sqrt(abs(m1$residuals))
  r2 <- sqrt(abs(m2$residuals))
  out1 <- r1 > median(r1) + s * IQR(r1)
  out2 <- r2 > median(r2) + s * IQR(r2)
  idxSEQ <- unique(c(which(out1), which(out2)))

  traj$error[idxSEQ] <- .updateError(traj$error[idxSEQ], rep("locSEQ", length(idxSEQ)))
  
  if (geo) {
    traj$lon[idxSEQ] <- NA
    traj$lat[idxSEQ] <- NA
    
    if (spline) {
      interpLon <- zoo::na.spline(traj$lon, x = traj$time, na.rm = FALSE)
      interpLat <- zoo::na.spline(traj$lat, x = traj$time, na.rm = FALSE)
    } else {
      interpLon <- zoo::na.approx(traj$lon, x = traj$time, na.rm = FALSE) 
      interpLat <- zoo::na.approx(traj$lat, x = traj$time, na.rm = FALSE) 
    }
    
    traj$lon[idxSEQ] <- interpLon[idxSEQ]
    traj$lat[idxSEQ] <- interpLat[idxSEQ]
  } else {
    traj$x[idxSEQ] <- NA
    traj$y[idxSEQ] <- NA
    
    if (spline) {
      interpX <- zoo::na.spline(traj$x, x = traj$time, na.rm = FALSE)
      interpY <- zoo::na.spline(traj$y, x = traj$time, na.rm = FALSE)
    } else {
      interpX <- zoo::na.approx(traj$x, x = traj$time, na.rm = FALSE)
      interpY <- zoo::na.approx(traj$y, x = traj$time, na.rm = FALSE)
    }
    
    traj$x[idxSEQ] <- interpX[idxSEQ]
    traj$y[idxSEQ] <- interpY[idxSEQ]
  }
  
  traj
} 


#' @title Correct NA locations in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  NA locations in trajectory tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#'
#' @param spline If \code{spline} is \code{TRUE}, NA locations are estimated 
#'  using spline interpolation. If \code{FALSE} (the default), a linear 
#'  interpolation is used instead.
#' 
#' @return A data table (from the \code{\link[data.table:data.table-package]{data.table}}
#'  package) with five columns:
#'  \itemize{
#'    \item{"id": }{the unique identifier of the trajectory.}
#'    \item{"x" or "lon": }{the x or longitude coordinates of the trajectory.}
#'    \item{"y" or "lat": }{the y or latitude coordinates of the trajectory.}
#'    \item{"time": }{the full timestamp (date+time) of each location in 
#'      \code{\link{POSIXct}} format.}
#'    \item{"error": }{the type of error detected and corrected (when possible) 
#'      by the function.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMEDUP}}, \code{\link{fixTIMESEQ}}, 
#'  \code{\link{fixTIMENA}}, \code{\link{fixLOCSEQ}}, \code{\link{fixMISSING}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixLOCNA <- function(traj, spline = FALSE) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  geo <- .isGeo(traj)
  
  if (geo) {
    idxNA <- is.na(traj$lon) | is.na(traj$lat)
    traj$lon[idxNA] <- NA
    traj$lat[idxNA] <- NA
    
    traj$error[idxNA] <- .updateError(traj$error[idxNA], rep("locNA", length(idxNA)))
    
    if (spline) {
      interpLon <- zoo::na.spline(traj$lon, x = traj$time, na.rm = FALSE)
      interpLat <- zoo::na.spline(traj$lat, x = traj$time, na.rm = FALSE)
    } else {
      interpLon <- zoo::na.approx(traj$lon, x = traj$time, na.rm = FALSE) 
      interpLat <- zoo::na.approx(traj$lat, x = traj$time, na.rm = FALSE) 
    }
    
    traj$lon[idxNA] <- interpLon[idxNA]
    traj$lat[idxNA] <- interpLat[idxNA]
  } else {
    idxNA <- is.na(traj$x) | is.na(traj$y)
    traj$x[idxNA] <- NA
    traj$y[idxNA] <- NA
    
    traj$error[idxNA] <- .updateError(traj$error[idxNA], rep("locNA", length(idxNA)))
    
    if (spline) {
      interpX <- zoo::na.spline(traj$x, x = traj$time, na.rm = FALSE)
      interpY <- zoo::na.spline(traj$y, x = traj$time, na.rm = FALSE)
    } else {
      interpX <- zoo::na.approx(traj$x, x = traj$time, na.rm = FALSE)
      interpY <- zoo::na.approx(traj$y, x = traj$time, na.rm = FALSE)
    }
    
    traj$x[idxNA] <- interpX[idxNA]
    traj$y[idxNA] <- interpY[idxNA]
  }
  
  traj
} 


#' @title Interpolate missing data in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  missing data (for instance due to writing errors) in trajectory tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#'  
#' @param begin A full timestamp (date+time) in \code{\link{POSIXct}} format
#'  corresponding to the beginning of the trajectory. If not set, it is set to 
#'  the first timestamp of the trajectory table.
#' 
#' @param end A full timestamp (date+time) in \code{\link{POSIXct}} format
#'  corresponding to the end of the trajectory. If not set, it is set to the 
#'  last timestamp of the trajectory table.
#' 
#' @param step A \code{\link{difftime}} object representing the time between two 
#'  consecutive locations of the trajectory. If not set, it is set to the most 
#'  common time difference between successive locations in \code{traj}.
#'
#' @param spline If \code{spline} is \code{TRUE}, inconsistent locations are 
#'  estimated using spline interpolation. If \code{FALSE} (the default), a linear 
#'  interpolation is used instead.
#' 
#' @return A data table (from the \code{\link[data.table:data.table-package]{data.table}}
#'  package) with five columns:
#'  \itemize{
#'    \item{"id": }{the unique identifier of the trajectory.}
#'    \item{"x" or "lon": }{the x or longitude coordinates of the trajectory.}
#'    \item{"y" or "lat": }{the y or latitude coordinates of the trajectory.}
#'    \item{"time": }{the full timestamp (date+time) of each location in 
#'      \code{\link{POSIXct}} format.}
#'    \item{"error": }{the type of error detected and corrected (when possible) 
#'      by the function.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMEDUP}}, \code{\link{fixTIMESEQ}}, 
#' \code{\link{fixTIMENA}}, \code{\link{fixLOCSEQ}}, \code{\link{fixLOCNA}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixMISSING <- function(traj, begin = NULL, end = NULL, step = NULL, spline = FALSE) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  id <- unique(traj$id)
  if (length(id) > 1) {
    stop("traj should have the same id for all observations.")
  }
  
  geo <- .isGeo(traj)
  
  if (is.null(step)) {
    d <- diff(traj$time)
    u <- units(d)
    step <- as.difftime(Mode(d)[1], units = u)
  }
  
  if (is.null(begin)) {
    begin <- min(traj$time, na.rm = TRUE)
  }
  
  if (is.null(end)) {
    end <- max(traj$time, na.rm = TRUE)
  }
  
  tmp <- data.table::data.table(time = seq(begin, end, step),
                                id = id)
  traj <- merge(traj, tmp, by = c("id", "time"), all = TRUE)
  traj$error[is.na(traj$error)] <- "MISSING"
  
  if (geo) {
    idxNA <- is.na(traj$lon) | is.na(traj$lat)
    traj$lon[idxNA] <- NA
    traj$lat[idxNA] <- NA
    
    if (spline) {
      interpLon <- zoo::na.spline(traj$lon, x = traj$time, na.rm = FALSE)
      interpLat <- zoo::na.spline(traj$lat, x = traj$time, na.rm = FALSE)
    } else {
      interpLon <- zoo::na.approx(traj$lon, x = traj$time, na.rm = FALSE) 
      interpLat <- zoo::na.approx(traj$lat, x = traj$time, na.rm = FALSE) 
    }
    
    traj$lon[idxNA] <- interpLon[idxNA]
    traj$lat[idxNA] <- interpLat[idxNA]
  } else {
    idxNA <- is.na(traj$x) | is.na(traj$y)
    traj$x[idxNA] <- NA
    traj$y[idxNA] <- NA
    
    if (spline) {
      interpX <- zoo::na.spline(traj$x, x = traj$time, na.rm = FALSE)
      interpY <- zoo::na.spline(traj$y, x = traj$time, na.rm = FALSE)
    } else {
      interpX <- zoo::na.approx(traj$x, x = traj$time, na.rm = FALSE)
      interpY <- zoo::na.approx(traj$y, x = traj$time, na.rm = FALSE)
    }
    
    traj$x[idxNA] <- interpX[idxNA]
    traj$y[idxNA] <- interpY[idxNA]
  }
  
  traj
}




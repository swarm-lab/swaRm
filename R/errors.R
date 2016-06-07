#' @title Find duplicated timestamps in a trajectory table
#' 
#' @description This function attempts to automatically detect duplicated 
#'  timestamps in trajectory tables. 
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @return A vector corresponding to the row numbers of the duplicated timestamps
#'  in the trajectory data table. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMEDUP}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findTIMEDUP <- function(traj) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  which(duplicated(traj$time) & !is.na(traj$time))
}


#' @title Find inconsistent timestamps in a trajectory table
#' 
#' @description This function attempts to automatically detect inconsistent 
#'  timestamps (for instance due to a writing error) in trajectory tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @return A vector corresponding to the row numbers of the inconsistent timestamps
#'  in the trajectory data table. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMESEQ}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findTIMESEQ <- function(traj) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  m <- suppressWarnings(MASS::rlm(as.numeric(traj$time) ~ c(1:length(traj$time)), maxit = 200))
  r <- sqrt(abs(m$residuals))
  pos <- as.numeric(names(r))
  out <- (r > median(r) + 3 * IQR(r)) | (r < median(r) - 3 * IQR(r))
  pos[out]
}


#' @title Find NA timestamps in a trajectory table
#' 
#' @description This function attempts to automatically detect NA timestamps in 
#'  trajectory tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @return A vector corresponding to the row numbers of the NA timestamps in the 
#'  trajectory data table. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTIMENA}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findTIMENA <- function(traj) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  which(is.na(traj$time))
}


#' @title Find inconsistent locations in a trajectory table
#' 
#' @description This function attempts to automatically detect inconsistent 
#'  locations (for instance due to a writing error) in trajectory tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#'  
#' @param s The discrimination threshold of the outlier detection algorithm. 
#'  Higher values correspond to less outliers.  
#' 
#' @return A vector corresponding to the row numbers of the inconsistent 
#'  locations in the trajectory data table. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixLOCSEQ}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findLOCSEQ <- function(traj, s = 15) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (.isGeo(traj)) {
    m1 <- loess(lon ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- abs(residuals(m1))
    r[r == 0] <- min(r[r > 0])
    m1 <- loess(lon ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
    m2 <- loess(lat ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- abs(residuals(m2))
    r[r == 0] <- min(r[r > 0])
    m2 <- loess(lat ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
  } else {
    m1 <- loess(x ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- abs(residuals(m1))
    r[r == 0] <- min(r[r > 0])
    m1 <- loess(x ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
    m2 <- loess(y ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- abs(residuals(m2))
    r[r == 0] <- min(r[r > 0])
    m2 <- loess(y ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
  }
  
  r1 <- sqrt(abs(m1$residuals))
  r2 <- sqrt(abs(m2$residuals))
  out1 <- r1 > median(r1) + s * IQR(r1)
  out2 <- r2 > median(r2) + s * IQR(r2)
  unique(c(which(out1), which(out2)))
} 


#' @title Find NA locations in a trajectory table
#' 
#' @description This function attempts to automatically detect NA locations in 
#'  trajectory tables.  
#' 
#' @param traj A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @return A vector corresponding to the row numbers of the NA locations in the 
#'  trajectory data table. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixLOCNA}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findLOCNA <- function(traj) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (.isGeo(traj)) 
    is.na(traj$lon) | is.na(traj$lat)
  else 
    is.na(traj$x) | is.na(traj$y)
} 


#' @title Find missing data in a trajectory table
#' 
#' @description This function attempts to automatically detect missing data (for 
#'  instance due to writing errors) in trajectory tables.  
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
#' @return A vector of the timestamps of the missing data points
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixMISSING}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findMISSING <- function(traj, begin = NULL, end = NULL, step = NULL) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  id <- unique(traj$id)
  if (length(id) > 1) {
    stop("traj should have the same id for all observations.")
  }
  
  if (is.null(step)) {
    d <- diff(traj$time)
    u <- units(d)
    step <- as.difftime(.Mode(d)[1], units = u)
  }
  
  if (is.null(begin)) {
    begin <- min(traj$time, na.rm = TRUE)
  }
  
  if (is.null(end)) {
    end <- max(traj$time, na.rm = TRUE)
  }
  
  tmp <- data.table::data.table(time = seq(begin, end, step), id = id)
  traj <- merge(traj, tmp, by = c("id", "time"), all = TRUE)
  
  if (.isGeo(traj)) 
    idxMISSING <- is.na(traj$lon)
  else
    idxMISSING <- is.na(traj$x)
  
  traj$time[idxMISSING]
}



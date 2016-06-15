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
#' @return A data frame with 2 columns:
#'  \itemize{
#'    \item{"time": }{the missing timestamp.}
#'    \item{"type": }{"MISSING" if the corresponding observation was not recorded; 
#'      "NA" if the corresponding observation was replaced by NA during the data
#'      recording or importing process.}
#'  }
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixMissing}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findMissing <- function(traj, begin = NULL, end = NULL, step = NULL) {
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
    step <- as.difftime(swaRm:::.Mode(d)[1], units = u)
  }
  
  if (is.null(begin)) {
    begin <- min(traj$time, na.rm = TRUE)
  }
  
  if (is.null(end)) {
    end <- max(traj$time, na.rm = TRUE)
  }
  
  full_seq <- seq(begin, end, step)
  my_seq <- traj$time
  my_seq <- my_seq[!(duplicated(my_seq) & !is.na(my_seq))]
  
  m1 <- match(full_seq, my_seq)
  missing <- data.frame(time = full_seq[which(is.na(m1))], type = NA)
  
  for (i in 1:nrow(missing)) {
    idx <- which(full_seq == missing$time[i])
    
    if (is.na(my_seq[idx])) {
      missing$type[i] <- "NA"
    } else {
      my_seq <- c(my_seq[1:(idx - 1)], missing$time[i], my_seq[idx:length(my_seq)])
      missing$type[i] <- "MISSING"
    }
  }
  
  missing
}


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
#' @note If a timestamps is missing where the duplicated timestamp is, this 
#'  function will replace the duplicated timestamp with the missing one. 
#'  Otherwise, the duplicated timestamp will be replaced by NA. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixTimeDup}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findTimeDup <- function(traj) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  which(duplicated(traj$time) & !is.na(traj$time))
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
#' @seealso \code{\link{makeTraj}}, \code{\link{fixLocErr}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findLocErr <- function(traj, s = 15) {
  if (!(.isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (.isGeo(traj)) {
    nas1 <- is.na(traj$lon) | is.na(traj$time)
    m1 <- loess(lon ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- rep(NA, nrow(traj))
    r[!nas1] <- abs(residuals(m1))
    r[r == 0] <- min(r[r > 0])
    m1 <- loess(lon ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
    
    nas2 <- is.na(traj$lat) | is.na(traj$time)
    m2 <- loess(lat ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- rep(NA, nrow(traj))
    r[!nas2] <- abs(residuals(m2))
    r[r == 0] <- min(r[r > 0])
    m2 <- loess(lat ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
  } else {
    nas1 <- is.na(traj$x) | is.na(traj$time)
    m1 <- loess(x ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- rep(NA, nrow(traj))
    r[!nas1] <- abs(residuals(m1))
    r[r == 0] <- min(r[r > 0])
    m1 <- loess(x ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
    
    nas2 <- is.na(traj$y) | is.na(traj$time)
    m2 <- loess(y ~ as.numeric(time), data = traj, span = 0.05, degree = 2)
    r <- rep(NA, nrow(traj))
    r[!nas2] <- abs(residuals(m2))
    r[r == 0] <- min(r[r > 0])
    m2 <- loess(y ~ as.numeric(time), data = traj, span = 0.05, degree = 2, weights = 1 / r)
  }
  
  r1 <- sqrt(abs(m1$residuals))
  r2 <- sqrt(abs(m2$residuals))
  out1 <- rep(NA, nrow(traj))
  out1[!nas1] <- r1 > median(r1) + s * IQR(r1)
  out2 <- rep(NA, nrow(traj))
  out2[!nas2] <- r2 > median(r2) + s * IQR(r2)
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



#' @title Find missing data in a trajectory table
#' 
#' @description This function attempts to automatically detect missing data (for 
#'  instance due to writing errors) in trajectory tables.  
#' 
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
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
findMissing <- function(x, begin = NULL, end = NULL, step = NULL) {
  if (!(is.trackTable(x))) 
    stop("x is not a trackTable object.")
  
  if (length(unique(x$id)) > 1) 
    stop("x should have the same id for all observations.")
  
  if (is.null(step)) {
    d <- diff(x[["time"]])
    u <- units(d)
    step <- as.difftime(.Mode(d)[1], units = u)
  }
  
  if (is.null(begin)) {
    begin <- min(x[["time"]], na.rm = TRUE)
  }
  
  if (is.null(end)) {
    end <- max(x[["time"]], na.rm = TRUE)
  }
  
  full_seq <- seq(begin, end, step)
  my_seq <- x[["time"]]
  my_seq <- my_seq[!(duplicated(my_seq) & !is.na(my_seq))]
  
  m1 <- match(full_seq, my_seq)
  idx <- which(is.na(m1))
  
  if (length(idx) > 0) {
    missing <- data.frame(time = full_seq[idx], type = NA)
    
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
  } else {
    idx
  }
}


#' @title Find duplicated timestamps in a trajectory table
#' 
#' @description This function attempts to automatically detect duplicated 
#'  timestamps in trajectory tables. 
#' 
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
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
findTimeDup <- function(x) {
  if (!(is.trackTable(x))) 
    stop("x is not a trackTable object.")
  
  which(duplicated(x[["time"]]) & !is.na(x[["time"]]))
}


#' @title Find inconsistent locations in a trajectory table
#' 
#' @description This function attempts to automatically detect inconsistent 
#'  locations (for instance due to a writing error) in trajectory tables.  
#' 
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
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
findLocErr <- function(x, s = 15) {
  if (!(is.trackTable(x))) 
    stop("x is not a trackTable object.")
  
  if (is.geo(x)) {
    nas1 <- is.na(x$lon) | is.na(x$time)
    m1 <- stats::loess(lon ~ as.numeric(time), data = x, span = 0.05, degree = 2)
    r <- rep(NA, nrow(x))
    r[!nas1] <- abs(stats::residuals(m1))
    r[r == 0] <- min(r[r > 0])
    m1 <- stats::loess(lon ~ as.numeric(time), data = x, span = 0.05, degree = 2, weights = 1 / r)
    
    nas2 <- is.na(x$lat) | is.na(x$time)
    m2 <- stats::loess(lat ~ as.numeric(time), data = x, span = 0.05, degree = 2)
    r <- rep(NA, nrow(x))
    r[!nas2] <- abs(stats::residuals(m2))
    r[r == 0] <- min(r[r > 0])
    m2 <- stats::loess(lat ~ as.numeric(time), data = x, span = 0.05, degree = 2, weights = 1 / r)
  } else {
    nas1 <- is.na(x$x) | is.na(x$time)
    m1 <- stats::loess(x ~ as.numeric(time), data = x, span = 0.05, degree = 2)
    r <- rep(NA, nrow(x))
    r[!nas1] <- abs(stats::residuals(m1))
    r[r == 0] <- min(r[r > 0])
    m1 <- stats::loess(x ~ as.numeric(time), data = x, span = 0.05, degree = 2, weights = 1 / r)
    
    nas2 <- is.na(x$y) | is.na(x$time)
    m2 <- stats::loess(y ~ as.numeric(time), data = x, span = 0.05, degree = 2)
    r <- rep(NA, nrow(x))
    r[!nas2] <- abs(stats::residuals(m2))
    r[r == 0] <- min(r[r > 0])
    m2 <- stats::loess(y ~ as.numeric(time), data = x, span = 0.05, degree = 2, weights = 1 / r)
  }
  
  r1 <- sqrt(abs(m1$residuals))
  r2 <- sqrt(abs(m2$residuals))
  out1 <- rep(NA, nrow(x))
  out1[!nas1] <- r1 > stats::median(r1) + s * stats::IQR(r1)
  out2 <- rep(NA, nrow(x))
  out2[!nas2] <- r2 > stats::median(r2) + s * stats::IQR(r2)
  unique(c(which(out1), which(out2)))
} 


#' @title Find NA locations in a trajectory table
#' 
#' @description This function attempts to automatically detect NA locations in 
#'  trajectory tables.  
#' 
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @return A vector corresponding to the row numbers of the NA locations in the 
#'  trajectory data table. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{fixLocNA}}
#' 
#' @examples
#' # TODO
#' 
#' @export
findLocNA <- function(x) {
  if (!(is.trackTable(x))) 
    stop("x is not a trackTable object.")
  
  if (is.geo(x)) 
    which(is.na(x$lon) | is.na(x$lat))
  else 
    which(is.na(x$x) | is.na(x$y))
} 

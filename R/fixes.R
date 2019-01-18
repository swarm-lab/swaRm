#' @title Interpolate missing data in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  missing data (for instance due to writing errors) in trajectory tables.  
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
#' @param spline If \code{spline} is \code{TRUE}, inconsistent locations are 
#'  estimated using spline interpolation. If \code{FALSE} (the default), a linear 
#'  interpolation is used instead.
#' 
#' @return A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function. If not present, an "error" column will be added that indicates 
#'  which observations were corrected.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{findMissing}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixMissing <- function(x, begin = NULL, end = NULL, step = NULL, spline = FALSE) {
  if (!(is.trackTable(x))) 
    stop("x is not a trackTable object.")
  
  if (!tibble::has_name(x, "error")) 
    x$error <- rep("OK", nrow(x))
  
  if (length(unique(x$id)) > 1) 
    stop("x should have the same id for all observations.")
  
  missing <- findMissing(x, begin = begin, end = end, step = step)
  
  if (length(missing) > 0) {
    for (i in 1:nrow(missing)) {
      idx <- which((x$time + 1) == missing$time[i]) + 1
      
      if (missing$type[i] == "MISSING") {
        if (is.geo(x)) { 
          x <- tibble::add_row(x, .after = idx - 1, 
                               id = x$id[idx - 1], time = missing$time[i], 
                               lon = NA, lat = NA, 
                               error = "MISSING") 
        } else {
          x <- tibble::add_row(x, .after = idx - 1, 
                               id = x$id[idx - 1], time = missing$time[i], 
                               x = NA, y = NA, 
                               error = "MISSING") 
        }
      } else {
        x$time[idx] <- missing$time[i]
        x$error[idx] <- swaRm:::.updateError(x$error[idx], "NA")
      }
    }
  }
  
  x
}


#' @title Correct duplicated timestamps in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  duplicated timestamps in trajectory tables. If it cannot correct a duplicated
#'  timestamp, it replaces it with NA. 
#' 
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#' 
#' @param step A \code{\link{difftime}} object representing the time between two 
#'  consecutive locations of the trajectory. If not set, it is set to the most 
#'  common time difference between successive locations in \code{traj}.
#' 
#' @return A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function. If not present, an "error" column will be added that indicates 
#'  which observations were corrected.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{findTimeDup}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixTimeDup <- function(x, step = NULL) {
  if (!(is.trackTable(x))) 
    stop("x is not a trackTable object.")
  
  if (!tibble::has_name(x, "error")) 
    x$error <- rep("OK", nrow(x))
  
  if (is.null(step)) {
    d <- diff(x$time)
    u <- units(d)
    step <- as.difftime(.Mode(d)[1], units = u)
  }
  
  idx_DUP <- findTimeDup(x)
  
  x$error[idx_DUP] <- .updateError(x$error[idx_DUP], rep("TIMEDUP", length(idx_DUP)))
  
  resolved <- !((x$time[idx_DUP - 1] + step) %in% x$time)
  x$time[idx_DUP[resolved]] <- x$time[idx_DUP[resolved] - 1] + step
  x$time[idx_DUP[!resolved]] <- NA

  x
}


#' @title Correct inconsistent locations in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  inconsistent locations (for instance due to a writing error) in trajectory 
#'  tables.  
#' 
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#'  
#' @param s The discrmination threshold of the outlier detection algorithm. 
#'  Higher values correspond to less outliers.  
#'
#' @param spline If \code{spline} is \code{TRUE}, inconsistent locations are 
#'  estimated using spline interpolation. If \code{FALSE} (the default), a linear 
#'  interpolation is used instead.
#' 
#' @return A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function. If not present, an "error" column will be added that indicates 
#'  which observations were corrected.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{findLocErr}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixLocErr <- function(x, s = 15, spline = FALSE) {
  if (!(is.trackTable(x))) 
    stop("x is not a trackTable object.")
  
  if (!tibble::has_name(x, "error")) 
    x$error <- rep("OK", nrow(x))
  
  idx_SEQ <- findLocErr(x, s = s)
  
  x$error[idx_SEQ] <- .updateError(x$error[idx_SEQ], rep("lOCERROR", length(idx_SEQ)))
  
  if (is.geo(x)) {
    x$lon[idx_SEQ] <- NA
    x$lat[idx_SEQ] <- NA
    
    if (spline) {
      interpLon <- zoo::na.spline(x$lon, x = x$time, na.rm = FALSE)
      interpLat <- zoo::na.spline(x$lat, x = x$time, na.rm = FALSE)
    } else {
      interpLon <- zoo::na.approx(x$lon, x = x$time, na.rm = FALSE) 
      interpLat <- zoo::na.approx(x$lat, x = x$time, na.rm = FALSE) 
    }
    
    x$lon[idx_SEQ] <- interpLon[idx_SEQ]
    x$lat[idx_SEQ] <- interpLat[idx_SEQ]
  } else {
    x$x[idx_SEQ] <- NA
    x$y[idx_SEQ] <- NA
    
    if (spline) {
      interpX <- zoo::na.spline(x$x, x = x$time, na.rm = FALSE)
      interpY <- zoo::na.spline(x$y, x = x$time, na.rm = FALSE)
    } else {
      interpX <- zoo::na.approx(x$x, x = x$time, na.rm = FALSE)
      interpY <- zoo::na.approx(x$y, x = x$time, na.rm = FALSE)
    }
    
    x$x[idx_SEQ] <- interpX[idx_SEQ]
    x$y[idx_SEQ] <- interpY[idx_SEQ]
  }
  
  x
} 


#' @title Correct NA locations in a trajectory table
#' 
#' @description This function attempts to automatically detect and correct 
#'  NA locations in trajectory tables.  
#' 
#' @param x A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function.
#'
#' @param spline If \code{spline} is \code{TRUE}, NA locations are estimated 
#'  using spline interpolation. If \code{FALSE} (the default), a linear 
#'  interpolation is used instead.
#' 
#' @return A trajectory data table as produced by the \code{\link{makeTraj}}
#'  function. If not present, an "error" column will be added that indicates 
#'  which observations were corrected.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{makeTraj}}, \code{\link{findLocNA}}
#' 
#' @examples
#' # TODO
#' 
#' @export
fixLocNA <- function(x, spline = FALSE) {
  if (!(is.trackTable(x))) 
    stop("x is not a trackTable object.")
  
  if (!tibble::has_name(x, "error")) 
    x$error <- rep("OK", nrow(x))
  
  idxNA <- findLocNA(x)
  
  if (is.geo(x)) {
    x$lon[idxNA] <- NA
    x$lat[idxNA] <- NA
    
    x$error[idxNA] <- .updateError(x$error[idxNA], rep("LOCNA", length(idxNA)))
    
    if (spline) {
      interpLon <- zoo::na.spline(x$lon, x = x$time, na.rm = FALSE)
      interpLat <- zoo::na.spline(x$lat, x = x$time, na.rm = FALSE)
    } else {
      interpLon <- zoo::na.approx(x$lon, x = x$time, na.rm = FALSE) 
      interpLat <- zoo::na.approx(x$lat, x = x$time, na.rm = FALSE) 
    }
    
    x$lon[idxNA] <- interpLon[idxNA]
    x$lat[idxNA] <- interpLat[idxNA]
  } else {
    x$x[idxNA] <- NA
    x$y[idxNA] <- NA
    
    x$error[idxNA] <- .updateError(x$error[idxNA], rep("LOCNA", length(idxNA)))
    
    if (spline) {
      interpX <- zoo::na.spline(x$x, x = x$time, na.rm = FALSE)
      interpY <- zoo::na.spline(x$y, x = x$time, na.rm = FALSE)
    } else {
      interpX <- zoo::na.approx(x$x, x = x$time, na.rm = FALSE)
      interpY <- zoo::na.approx(x$y, x = x$time, na.rm = FALSE)
    }
    
    x$x[idxNA] <- interpX[idxNA]
    x$y[idxNA] <- interpY[idxNA]
  }
  
  x
} 

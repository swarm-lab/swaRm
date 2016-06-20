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
fixMissing <- function(traj, begin = NULL, end = NULL, step = NULL, spline = FALSE) {
  if (!(isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (is.null(traj$error)) {
    traj[["error"]] <- rep("OK", nrow(traj))
  }
  
  id <- unique(traj[["id"]])
  if (length(id) > 1) {
    stop("traj should have the same id for all observations.")
  }
  
  missing <- findMissing(traj, begin = begin, end = end, step = step)
  
  if (length(missing) > 0) {
    for (i in 1:nrow(missing)) {
      idx <- which((traj[["time"]] + 1) == missing$time[i]) + 1
      
      if (missing$type[i] == "MISSING") {
        tmp <- traj[1, ]
        tmp[["time"]] <- missing$time[i]
        
        if (isGeo(traj)) {
          tmp[["lon"]] <- NA
          tmp[["lat"]] <- NA
        } else {
          tmp[["x"]] <- NA
          tmp[["y"]] <- NA
        }
        
        tmp[["error"]] <- .updateError(tmp[["error"]], "MISSING")
        traj$data <- rbind(traj$data[1:(idx - 1), ], tmp$data, traj$data[idx:nrow(traj), ])
      } else {
        traj[["time"]][idx] <- missing$time[i]
        traj[["error"]][idx] <- .updateError(traj[["error"]][idx], "NA")
      }
    }
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
fixTimeDup <- function(traj, step = NULL) {
  if (!(isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (is.null(traj[["error"]])) {
    traj[["error"]] <- rep("OK", nrow(traj))
  }
  
  if (is.null(step)) {
    d <- diff(traj[["time"]])
    u <- units(d)
    step <- as.difftime(.Mode(d)[1], units = u)
  }
  
  idx_DUP <- findTimeDup(traj)
  
  traj[["error"]][idx_DUP] <- .updateError(traj[["error"]][idx_DUP], rep("TIMEDUP", length(idx_DUP)))
  
  resolved <- !((traj[["time"]][idx_DUP - 1] + step) %in% traj[["time"]])
  traj[["time"]][idx_DUP[resolved]] <- traj[["time"]][idx_DUP[resolved] - 1] + step
  traj[["time"]][idx_DUP[!resolved]] <- NA

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
fixLocErr <- function(traj, s = 15, spline = FALSE) {
  if (!(isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (is.null(traj[["error"]])) {
    traj[["error"]] <- rep("OK", nrow(traj))
  }
  
  idx_SEQ <- findLocErr(traj, s = s)
  
  traj[["error"]][idx_SEQ] <- .updateError(traj[["error"]][idx_SEQ], rep("lOCERROR", length(idx_SEQ)))
  
  if (isGeo(traj)) {
    traj[["lon"]][idx_SEQ] <- NA
    traj[["lat"]][idx_SEQ] <- NA
    
    if (spline) {
      interpLon <- zoo::na.spline(traj[["lon"]], x = traj[["time"]], na.rm = FALSE)
      interpLat <- zoo::na.spline(traj[["lat"]], x = traj[["time"]], na.rm = FALSE)
    } else {
      interpLon <- zoo::na.approx(traj[["lon"]], x = traj[["time"]], na.rm = FALSE) 
      interpLat <- zoo::na.approx(traj[["lat"]], x = traj[["time"]], na.rm = FALSE) 
    }
    
    traj[["lon"]][idx_SEQ] <- interpLon[idx_SEQ]
    traj[["lat"]][idx_SEQ] <- interpLat[idx_SEQ]
  } else {
    traj[["x"]][idx_SEQ] <- NA
    traj[["y"]][idx_SEQ] <- NA
    
    if (spline) {
      interpX <- zoo::na.spline(traj[["x"]], x = traj[["time"]], na.rm = FALSE)
      interpY <- zoo::na.spline(traj[["y"]], x = traj[["time"]], na.rm = FALSE)
    } else {
      interpX <- zoo::na.approx(traj[["x"]], x = traj[["time"]], na.rm = FALSE)
      interpY <- zoo::na.approx(traj[["y"]], x = traj[["time"]], na.rm = FALSE)
    }
    
    traj[["x"]][idx_SEQ] <- interpX[idx_SEQ]
    traj[["y"]][idx_SEQ] <- interpY[idx_SEQ]
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
fixLocNA <- function(traj, spline = FALSE) {
  if (!(isTraj(traj))) {
    stop("traj should be a trajectory data table as produced by the makeTraj function.")
  }
  
  if (is.null(traj[["error"]])) {
    traj[["error"]] <- rep("OK", nrow(traj))
  }
  
  idxNA <- findLocNA(traj)
  
  if (isGeo(traj)) {
    traj[["lon"]][idxNA] <- NA
    traj[["lat"]][idxNA] <- NA
    
    traj[["error"]][idxNA] <- .updateError(traj[["error"]][idxNA], rep("lOCNA", length(idxNA)))
    
    if (spline) {
      interpLon <- zoo::na.spline(traj[["lon"]], x = traj[["time"]], na.rm = FALSE)
      interpLat <- zoo::na.spline(traj[["lat"]], x = traj[["time"]], na.rm = FALSE)
    } else {
      interpLon <- zoo::na.approx(traj[["lon"]], x = traj[["time"]], na.rm = FALSE) 
      interpLat <- zoo::na.approx(traj[["lat"]], x = traj[["time"]], na.rm = FALSE) 
    }
    
    traj[["lon"]][idxNA] <- interpLon[idxNA]
    traj[["lat"]][idxNA] <- interpLat[idxNA]
  } else {
    traj[["x"]][idxNA] <- NA
    traj[["y"]][idxNA] <- NA
    
    traj[["error"]][idxNA] <- .updateError(traj[["error"]][idxNA], rep("lOCNA", length(idxNA)))
    
    if (spline) {
      interpX <- zoo::na.spline(traj[["x"]], x = traj[["time"]], na.rm = FALSE)
      interpY <- zoo::na.spline(traj[["y"]], x = traj[["time"]], na.rm = FALSE)
    } else {
      interpX <- zoo::na.approx(traj[["x"]], x = traj[["time"]], na.rm = FALSE)
      interpY <- zoo::na.approx(traj[["y"]], x = traj[["time"]], na.rm = FALSE)
    }
    
    traj[["x"]][idxNA] <- interpX[idxNA]
    traj[["y"]][idxNA] <- interpY[idxNA]
  }
  
  traj
} 



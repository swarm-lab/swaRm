#' @title Centroid of a set of coordinates
#' 
#' @description This function computes the centroid of a set of x-y (or 
#'  longitude-latitude) coordinates. 
#' 
#' @param x A vector of x (or longitude) coordinates.
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @param robust A logical value indicating whether to compute the centroid as a
#'  simple average of the coordinates (FALSE, the default), or as the average of 
#'  the coordinates weighted by the inversd of their mean pairwise distance to 
#'  all other coordinates in the set (TRUE).  
#' 
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#' 
#' @return A two-element vector corresponding to the location of the centroid.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{dist2centroid}}
#' 
#' @examples
#' # TODO
#' 
#' @export
centroid <- function(x, y, robust = FALSE, geo = FALSE) {
  if (length(x) != length(y)) 
    stop("x and y should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  if (robust) {
    pd <- pdist(x, y, geo = geo)
    diag(pd) <- NA
    w <- 1 / colMeans(pd, na.rm = TRUE)
  }

  if (geo) {
    if (length(x) > 1) {
      if (robust) {
        centrd <- tibble::as_tibble(geosphere::geomean(cbind(x, y), w))
      } else {
        centrd <- tibble::as_tibble(geosphere::geomean(cbind(x, y)))
      }
      names(centrd) <- c("lon", "lat")
    } else {
      centrd <- tibble::tibble(lon = x, lat = y)
    }
  } else {
    if (robust) {
      centrd <- tibble::tibble(x = stats::weighted.mean(x, w, na.rm = TRUE), 
                               y = stats::weighted.mean(y, w, na.rm = TRUE))
    } else {
      centrd <- tibble::tibble(x = mean(x, na.rm = TRUE), 
                               y = mean(y, na.rm = TRUE))
    }
  }
  
  centrd
}

#' @title Distance to the centroid of a set of coordinates
#' 
#' @description Given a set of x-y (or longitude-latitude) coordinates, this 
#'  function computes their distance to the centroid of the set.
#' 
#' @param x A vector of x (or longitude) coordinates.
#' 
#' @param y A vector of y (or latitude) coordinates.
#' 
#' @param robust A logical value indicating whether to compute the centroid as a
#'  simple average of the coordinates (FALSE, the default), or as the average of 
#'  the coordinates weighted by the inversd of their mean pairwise distance to 
#'  all other coordinates in the set (TRUE).    
#' 
#' @param geo A logical value indicating whether the locations are defined by 
#'  geographic coordinates (pairs of longitude/latitude values). Default: FALSE.
#' 
#' @return A vector of the same length as x and y corresponding to the 
#'  individual distance of each point to the centroid of the set.
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#'  
#' @examples
#' # TODO
#' 
#' @export
dist2centroid <- function(x, y, robust = FALSE, geo = FALSE) {
  centrd <- as.matrix(centroid(x, y, robust = robust, geo = geo))
  
  if (geo) {
    m1 <- matrix(centrd, nrow = length(x), ncol = 2, byrow = TRUE)
    m2 <- cbind(x, y)
    geosphere::distGeo(m1, m2)
  } else {
    sqrt((x - centrd[1]) ^ 2 + (y - centrd[2]) ^ 2)
  }
}

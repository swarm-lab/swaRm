#' @title Sphericity
#'
#' @description Given a set of locations, this function approximates the 
#'  sphericity of the set by calculating the bivariate 95% confidence ellipse of 
#'  the set. 
#' 
#' @param x A vector of x coordinates. 
#' 
#' @param y A vector of y coordinates.
#' 
#' @return A single numeric value corresponding to the ratio between the minor 
#'  and major axis of the bivariate 95% confidence ellipse of the set. A value 
#'  close to 1 indicates that the set is approximately circular; a value close 
#'  to 0 indicates that the set is strongly elongated. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{stretch}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' sphericity(x, y)
#' 
#' @export
sphericity <- function(x, y) {
  if (length(x) != length(y)) 
    stop("x and y should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  tryCatch({
    ell <- .ellipse(x, y)
    ell$b / ell$a}, 
    error = function(e) as.numeric(NA))
}


#' @title Stretching Direction
#'
#' @description Given a set of locations, this function approximates the 
#'  stretching direction of the set by calculating the angle of the main axis of 
#'  the bivariate 95% confidence ellipse of the set. 
#' 
#' @param x A vector of x coordinates. 
#' 
#' @param y A vector of y coordinates.
#' 
#' @return A single numeric value corresponding to the angle (in radians) of the 
#'  main axis of the bivariate 95% confidence ellipse of the set. 
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{sphericity}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' stretch(x, y)
#'
#' @export
stretch <- function(x, y) {
  if (length(x) != length(y)) 
    stop("x and y should have the same length.")
  
  if (!is.numeric(x) | !is.numeric(y))
    stop("x and y should be numeric.")
  
  tryCatch({
    ell <- .ellipse(x, y)
    ell$alpha}, 
    error = function(e) as.numeric(NA))
}


#' @title Sinuosity
#' 
#' @description Given a set of successive step lengths and turning angles, this
#'  function computes he sinuosity of a trajectory as defined by Benhamou (2004), 
#'  equation 8.  This is a corrected version of the sinuosity index defined in 
#'  Bovet & Benhamou (1988), which is suitable for a wider range of turning 
#'  angle distributions, and does not require a constant step length.
#'
#' @param step_lengths A vector of step lengths as calculated by 
#'  \code{\link{linear_dist}}. 
#' 
#' @param turning_angles A vector of turning angles as calculated by 
#'  \code{\link{ang_speed}}. 
#'
#' @return A single numeric value corresponding to the sinuosity of the 
#'  trajectory. 
#' 
#' @references Benhamou, S. (2004). How to reliably estimate the tortuosity of 
#'  an animal’s path: straightness, sinuosity, or fractal dimension? Journal of 
#'  Theoretical Biology, 229(2), 209–220. https://doi.org/10.1016/j.jtbi.2004.03.016
#'  
#'  Bovet, P., & Benhamou, S. (1988). Spatial analysis of animals’ movements 
#'  using a correlated random walk model. Journal of Theoretical Biology, 131(4), 
#'  419–433. https://doi.org/10.1016/S0022-5193(88)80038-9
#' 
#' @author Simon Garnier, \email{garnier@@njit.edu}
#' 
#' @seealso \code{\link{linear_dist}}, \code{\link{ang_speed}}
#' 
#' @examples
#' x <- rnorm(25)
#' y <- rnorm(25, sd = 3)
#' t <- as.POSIXct(1:25, origin = Sys.time())
#' step_lengths <- linear_dist(x, y)
#' turning_angles <- ang_speed(x, y, t)
#' sinuosity(step_lengths, turning_angles)
#' 
#' @export
sinuosity <- function(step_lengths, turning_angles) {
  if (length(step_lengths) != length(turning_angles)) 
    stop("step_lengths and turning_angles should have the same length.")
  
  nas <- is.na(step_lengths) | is.na(turning_angles)
  mean_sl <- mean(step_lengths[!nas])
  coefvar_sl <- stats::sd(step_lengths[!nas]) / mean_sl
  meancos_ta <- mean(cos(turning_angles[!nas]))
  2 / sqrt(mean_sl * (((1 + meancos_ta) / (1 - meancos_ta)) + coefvar_sl ^ 2))
}
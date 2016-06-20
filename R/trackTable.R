#' @export
trackTable <- R6::R6Class(
  classname = "trackTable",
  inherit = R6Frame::R6Frame,
  
  private = list(.geo = NULL, .type = NULL),
  
  public = list(
    setGeo = function(val) {
      if (!is.logical(val))
        stop("val must be a logical value.")
      
      .geo <<- val
    }, 
    
    getGeo = function() .geo,
    
    setType = function(val) {
      if (!(val %in% c("2D", "3D")))
        stop("val must be either 2D or 3D.")
      
      .type <<- val
    },
    
    getType = function() .type,
    
    print = function(...) {
      cat("Trajectory table [", nrow(self$data), " observations]\n", sep = "")
      cat("Number of tracks: ", length(unique(self$data$id)), "\n")
      cat("Geographic data: ", self$getGeo(), "\n")
      cat("Dimensions: ", self$getType(), "\n")
      cat("\n")
      print(self$data)
      
      invisible(self)
    }
  )
)


.trackTable <- function(..., geo = FALSE, type = "2D", keep.rownames = FALSE, 
                        check.names = FALSE, key = NULL) {
  # dots <- names(list(...))
  # for (i in dots) {
  #   fn <- eval(parse(
  #     text = paste0("function(val = NULL) {
  #                       if (is.null(val))
  #                         self$data$", i, "
  #                       else 
  #                         self$data$", i, " <- val}")))
  #   trackTable$set("active", i, fn)
  # }
  
  tt <- trackTable$new(
    data.table::data.table(..., keep.rownames = keep.rownames, 
                           check.names = check.names, key = key)
  )
  tt$setGeo(geo)
  tt$setType(type)
  tt
}


#' @export
rbindtt <- function(l) {
  if (!all(sapply(l, isTraj)))
    stop("All elements in the list must be trajectory tables as produced by the makeTraj function.")
  
  if ((length(unique(sapply(l, isGeo))) > 1) | (length(unique(sapply(l, function(x) x$getType()))) > 1))
    stop("All trajectories in the list must be of the same type.")

  tt <- trackTable$new(data.table::rbindlist(lapply(l, function(x) x$data)))
  tt$setGeo(l[[1]]$getGeo())
  tt$setType(l[[1]]$getType())
  tt
} 


#' @export
do_.R6Frame <- function(x, ...) {
  x$do(dplyr::do_, lazyeval::all_dots(...), env = parent.frame())
}


#' @export
merge.R6Frame <- function(x, ...) {
  x$do_merge(merge, list(...), env = parent.frame())
}






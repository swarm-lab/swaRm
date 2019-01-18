#' @title Maintain Class After Modification
#' 
#' @description Copy class and attributes from the original version of an object 
#'  to a modified version.
#'
#' @param x The original object, which has a class/attributes to copy
#' 
#' @param result The modified object, which is / might be missing the class/attributes.
#'
#' @return \code{result}, now with class/attributes restored.
#' 
#' @export
reclass <- function(x, result) {
  UseMethod('reclass')
}

#' @export
reclass.default <- function(x, result) {
  class(result) <- unique(c(class(x)[[1]], class(result)))
  attr(result, class(x)[[1]]) <- attr(x, class(x)[[1]])
  result
}


#' @title Dplyr Methods For Track Tables
#' 
#' @name dplyr
#' 
#' @description \code{\link[dplyr]{dplyr}} methods for track tables objects.
#' 
#' @param .data A track table. 
#' 
#' @param ... Additional arguments to be passed to the corresponding 
#'  \code{\link[dplyr]{dplyr}} method. 
#' 
#' @seealso \code{\link[dplyr]{select}}, \code{\link[dplyr]{rename}},
#'  \code{\link[dplyr]{filter}}, \code{\link[dplyr]{arrange}}, \code{\link[dplyr]{mutate}},
#'  \code{\link[dplyr]{transmute}}, \code{\link[dplyr]{summarise}}, \code{\link[dplyr]{summarize}},
#'  \code{\link[dplyr]{group_by}}, \code{\link[dplyr]{ungroup}}, \code{\link[dplyr]{sample_n}},
#'  \code{\link[dplyr]{sample_frac}}, \code{\link[dplyr]{do}}

#' @rdname dplyr 
#' 
#' @export
select.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
rename.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
filter.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
arrange.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
mutate.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
transmute.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
summarise.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
summarize.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
group_by.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
ungroup.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
sample_n.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
sample_frac.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @rdname dplyr 
#' 
#' @export
do.trackTable <- function(.data, ...) reclass(.data, NextMethod())

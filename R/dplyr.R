#' Copy class and attributes from the original version of an object to a modified version.
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

#' @export
select.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
rename.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
filter.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
arrange.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
mutate.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
transmut.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
summarise.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
summarize.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
group_by.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
ungroup.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
sample_n.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
sample_frac.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' @export
do.trackTable <- function(.data, ...) reclass(.data, NextMethod())

#' Assign type attribute
#'
#' type description
#'
#' @param x asdfc
#' @param ... asf
#' @param value asdf
#'
#' @return sdf
#' @export
#'
#' @examples
`type<-` <- function(x, ..., value) {
  UseMethod("type<-")
}



#' Assign type attribute
#'
#' @param x asd
#' @param value asd
#' @param ... asd
#'
#' @return asd
#' @export
#'
#' @examples
`type<-.default` <- function(x, ..., value) {
  # value must be one of the following types
  if (is.na(charmatch(value, c("", "id", "y", "y2", "t", "t2", "e", "e2", "x")))) {
    stop('`value` must be one of "", "id", "y", "y2", "t", "t2", "e", "e2", "x"')
  }
  # Check value is character vector of length 1
  if (!is.character(value) | length(value) != 1L) {
    stop("value must be character vector of length 1")
  }
  # Assign value to type attribute
  attr(x, "type") <- value
  return(x)
}

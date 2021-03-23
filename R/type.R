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
#' @examples #asdf
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
#' @examples #asdf
`type<-.default` <- function(x, ..., value) {
  # value must be one of the following types
  if (is.na(charmatch(value, c("", "id", "y", "y2", "t", "t2", "e", "e2", "x", NA_character_)))) {
    stop('`value` must be one of "", "id", "y", "y2", "t", "t2", "e", "e2", "x", NA_character_')
  }
  # Check value is character vector of length 1
  if (!is.character(value) | length(value) != 1L) {
    stop("value must be character vector of length 1")
  }
  # Assign value to type attribute
  attr(x, "type") <- value
  return(x)
}





#' Add type attribute to data.frame
#'
#' @param x asd
#' @param fill asd
#' @param ... asd
#' @param value asd
#'
#' @return asd
#' @export
#'
#' @examples #asds
`type<-.data.frame` <- function(x, fill = NULL, ..., value) {
  if (is.null(fill)) {
    if (is.list(value)) {
      if (!all(names(x) %in% unlist(value))) {
        stop("'value' list must contain all variables in 'x' or specify a 'fill' value")
      } else {
        x[, ] <- lapply(names(x), function(y) {
          type(x[, y]) <- names(value)[grepl(y, value)]
          return(x[, y])
        })
      }
    } else {
      if (length(x) != length(value)) {
        stop("'value' vector must have the same length as 'x' or specify a 'fill' value")
      } else {
        if (is.null(names(value)) || any(!names(value) %in% names(x))) {
          if (any(!names(value) %in% names(x))) {
            message("Names of `values` do not match names of `x`, using index position matching")
          }
          # index position matching
          for (i in seq(x)) {
            type(x[[i]]) <- value[[i]]
          }
        } else {
          for (i in names(value)) {
            type(x[[i]]) <- value[[i]]
          }
        }
      }
    }
  } else {
    if (is.na(charmatch(fill, c("", "id", "y", "y2", "t", "t2", "e", "e2", "x", NA_character_)))) {
      stop('`fill` must be one of "", "id", "y", "y2", "t", "t2", "e", "e2", "x", NA_character_')
    }
    if (is.list(fill)) {

    } else {
      if (is.null(names(value)) || any(!names(value) %in% names(x))) {
        if (any(!names(value) %in% names(x))) {
          message("Names of `values` do not match names of `x`, using index position matching")
        }
        for (i in seq(x)) {
          if (i <= length(value)) {
            type(x[[i]]) <- value[[i]]
          } else {
            type(x[[i]]) <- fill
          }
        }
      } else {
        for (i in names(x)) {
          if (i %in% names(value)) {
            type(x[[i]]) <- value[[i]]
          } else {
            type(x[[i]]) <- fill
          }
        }
      }
    }
  }
  return(x)
}





#' Get type attribute
#'
#' @param x asd
#' @param ... asd
#'
#' @return asd
#' @export
#'
#' @examples #asd
type <- function(x, ...) {
  UseMethod("type")
}




#' Get type attribute
#'
#' @param x asd
#' @param ... asd
#'
#' @return asd
#' @export
#'
#' @examples #asd
type.default <- function(x, ...) {
  at <- attributes(x)
  ty <- at[['type']]
  return(ty)
}



#' Get type attributes
#'
#' @param x asd
#' @param ... asd
#'
#' @return asd
#' @export
#'
#' @examples #asd
type.data.frame <- function(x, ...) {
  types <- mapply(x, FUN = type, USE.NAMES=FALSE)
  names(types) <- names(x)
  return(types)
}

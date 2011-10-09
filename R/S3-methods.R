##' @include S4-methods.R
NULL

## Methods using a previously defined S3 generic
## Mostly data store methods

##' Length of object
##'
##' @param x object
##' @return length of object
##' @export
##' @rdname gWidgets-S3methods
length.GComponent <- function(x) {
  if(isExtant(x))
    x$get_length()
}

##' set length of object
##'
##' @export
##' @rdname gWidgets-S3methods
"length<-.GComponent" <- function(x, value) {
  if(isExtant(x))
    x$set_length(value)
  x
}

##' dimension of object
##'
##' @export
##' @rdname gWidgets-S3methods
dim.GComponent <- function(x) {
  x$get_dim()
}


##' get names of object
##'
##' Names are used in many different contexts.
##' @export
##' @rdname gWidgets-S3methods
names.GComponent <- function(x) {
  if(isExtant(x))
    x$get_names()
}

##' set names of object
##'
##' @export
##' @rdname gWidgets-S3methods
"names<-.GComponent" <- function(x, value) {
  if(isExtant(x))
    x$set_names(value)
  x
}

##' Get dimnames of object
##'
##' @export
##' @rdname gWidgets-S3methods
dimnames.GComponent <- function(x) {
  if(isExtant(x))
    x$get_dimnames()
}

##' Set dimnames of object
##'
##' @export
##' @rdname gWidgets-S3methods
"dimnames<-.GComponent" <- function(x, value) {
  if(isExtant(x))
    x$set_dimnames(value)
  x
}

##' Get items of object
##'
##' @param x \code{GComponent} object
##' @param i main index or row index (if implemented)
##' @param j column index (if implemented)
##' @param ... ignored
##' @param drop logical. If \code{TRUE} return value is simplified, if possible. 
##' @export
##' @rdname gWidgets-S3methods
"[.GComponent" <- function(x, i, j, ..., drop=TRUE) {
  if(isExtant(x))
    x$get_items(i, j, ..., drop=TRUE)
}

##' Return children of a parent container
##'
##' @export
##' @rdname gWidgets-S3methods
"[.GContainer" <- function(x, i, j, ..., drop=TRUE) {
  if(isExtant(x))
    x$get_items(i, j, ..., drop=TRUE)
}

##' Set object's items
##'
##' @param value value to set
##' @export
##' @rdname gWidgets-S3methods
"[<-.GComponent" <- function(x, i, j, ..., value) {
  if(isExtant(x)) 
    x$set_items(value, i, j, ...)
  x
}

### This is an issue with the redesign
## ##' $ -- get property from underlying widget
## "$.GComponent" <- function(x, key, ...) x$get_property(key, ...)

## ##' $<- set property of underlying widget
## "$<-.GComponent" <- function(x, key, ..., value) {
##   x$set_property(key, ..., value=value)
##   x
## }

## "[[.GComponent" <- function(x, i, ...) 
##   x$get_property(i, ...)

## "[[<-.GComponent" <- function(x, i, ..., value)  {
##   x$set_property(key, ..., value=value)
##   x
## }

##' Call widgets \code{update_widget} method
##'
##' The update method will cause a widget to recompute itself, if it is necessary.
##' @export
##' @rdname gWidgets-S3methods
update.GComponent <- function(object, ...) {
  if(isExtant(object))
    object$update_widget(...)
}


##' str method for widgets
##'
##' @export
##' @rdname gWidgets2-S3methods
str.GComponent <- function(object, ...) cat(sprintf("Object of class %s\n", class(object)))


##' merge two lists
##' 
##' @param x a list
##' @param y a list
##' @param overwrite logical should we overright values in x
##' @export
##' @rdname  gWidgets2-S3methods
merge.list <- function(x, y, overwrite=TRUE) {
  if(missing(y) || is.null(y))
    return(x)
  nms <- names(y)
  for(i in seq_along(y)) {
    nm <- nms[i]                        # possibly NULL
    if(is.null(nm)) {
      x[[length(x) + 1]] <- y[[i]]
    } else if(overwrite || !(nm %in% names(x))) {
      x[[nm]] <- y[[i]]
    }
  }
  x
}

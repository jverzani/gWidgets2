## miscellaneous functions

##' merge two lists
##' 
##' @param x a list
##' @param y a list
##' @param overwrite logical should we overright values in x
##' @export
##' @rdname  gWidgets2-S3methods
merge.list <- function(x, y, ...) {
  args <- list(...)
  overwrite <- getWithDefault(args$overwrite, TRUE)
  
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




##' Return x unless NULL, NA, length 0, ..., in which case we give default
##'
##' @param x value to return or its default
##' @param default default value
##' @return x or default
##' @export
getWithDefault <- function(x, default) {
  if(is_empty(x))
    default
  else
    x
}

##' is value missing, null, 0-length or NA length 1
##'
##' @param x object to test
##' @return logical
##' @export
is_empty <- function(x) {
  missing(x) ||
  is.null(x) ||
  (length(x) == 0) ||
  (is.atomic(x) && length(x) == 1 && is.na(x))
}

##' Functions to message something needs doing. Easy to search for
##'
##' @param msg optional message to emit
##' @export
XXX <- function(msg) {
  if(!missing(msg))
    message(msg)
}

##' Method to send message if any depreacted arguments are being used
##'
##' Many arguments were deprecated due to various reasons. This is meant to ease porting of code.
check_deprecated <- function(deprecated_args=list(), ...) {
  if(!length(deprecated_args))
    return()

  args <- list(...)
  sapply(names(args), function(i) {
    if(!is.null(tmp <- deprecated_args[[i, exact=TRUE]]))
      message(sprintf("Argument %s has been deprecated:\n\t o ",
                      i, paste(tmp, collapse="\n\t")))
  })
}

##' check that toolkit object return the right class
##'
##' The S3 dispatch assumes naming conventions in the class names. This offers some check.
check_return_class <- function(obj, ret_class) {
  if(!any(sapply(ret_class, is, object=obj)))
    stop(sprintf("Expecting toolkit object of class (or subclass) %s. Got one of class %s",
                 paste(ret_class, collapse="; "), class(obj)[1]))
}
  
##' blurb about installation
##'
##' put in so can be updated easily
installing_gWidgets_toolkits <- function() {
  file <- system.file("install/Installing_gWidgets_Toolkits.txt", package="gWidgets2")
  tmp <- readLines(file)
  for(i in tmp) cat(i,"\n")

}


##' Return logical indicating if we are on a macintosh machine
##'
##' @return logical
##' @export
is_MacOSX <- function() {
  grepl("darwin", R.Version()$os)
}

##' Return logical indicating if we are on a Windows machine
##'
##' @return logical
##' @export
is_Windows <- function() {
  grepl("Windows", R.Version()$os)
}
  
## some special class unions so we can have easier to deal with default
setClassUnion("IntegerOrNULL", c("integer", "NULL"))
setClassUnion("CharacterOrNULL", c("character", "NULL"))
setClassUnion("LogicalOrNULL", c("logical", "NULL"))
setClassUnion("LogicalCharacterOrNULL", c("logical", "character", "NULL"))

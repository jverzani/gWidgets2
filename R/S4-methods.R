##' @include misc.R
NULL

## Methods using a previously defined S4 generic


##'  Print method for widgets
##'
##' @exportMethod
##' @rdname gWidgets2-S4-methods
##' @docType methods
setMethod(show, signature="GComponent", function(object) cat(sprintf("Object of class %s\n", class(object))))


##' str method for widgets
##'
##' @exportMethod
##' @rdname gWidgets2-S4-methods
##' @docType methods
setMethod(str, signature="GComponent", function(object) cat(sprintf("Object of class %s\n", class(object))))


##' Print method for Observer
##'
##' @exportMethod
##' @rdname gWidgets2-S4-methods
##' @docType methods
setMethod(show, signature="Observer", function(object) cat("Observer instance"))


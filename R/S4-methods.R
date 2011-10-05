##' @include misc.R
NULL

## Methods using a previously defined S4 generic




##' Print method for Observer
##'
##' @exportMethod show
##' @rdname gWidgets2-S4-methods
##' @docType methods
setMethod(show, signature="Observer", function(object) cat("Observer instance"))


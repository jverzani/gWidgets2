##' @include methods.R
NULL

##' Widget for HTML display
##'
##' @param x url or character vector of HTML formatted text. URLs marked by "http://" prefix
##' @inheritParams gwidget
##' @return a \code{GHtml} instance.
##' @export
ghtml <- function(x, 
                  container=NULL,
                  ...,
                  toolkit=guiToolkit()) {
  
  x <- paste(x, collapse="\n")

  if(is.character(toolkit))
    toolkit <- guiToolkit(toolkit)
  
  obj <- .ghtml(toolkit, x, container, ...)

  check_return_class(obj, "GHtml")
  obj   
  
}

##' S3 generic whose methods are implemented in the toolkit packages
##'
##' @rdname ghtml
##' @export
##' @author john verzani
.ghtml <- function(toolkit, x, container=NULL, ...) UseMethod(".ghtml")


##' \code{svalue<-} method for a ghtml
##'
##' Use to update displayed content. Value is HTML fragment or url
##' @inheritParams svalue
##' @export
##' @rdname glabel
##' @method svalue<- GLabel
##' @S3method svalue<- GLabel
"svalue<-.GHtml" <- function(obj, index=TRUE,  ..., value) {
  value <- paste(value, collapse="\n")
  NextMethod()
}



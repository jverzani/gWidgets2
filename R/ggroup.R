##' @include methods.R
NULL

##' Basic box container
##'
##' @param horizontal logical. If TRUE, left to right layout, otherwise top to bottom
##' @param spacing spacing aroud widget 
##' @param use.scrollwindow logical. If TRUE, places box container in scrollwindow. In this case, a size should be set.
##' @param container parent container
##' @param ... passed to \code{add} method of parent container
##' @param toolkit toolkit for dispatch
##' @return a GGroup instance.
##' @export
##' @rdname ggroup
ggroup <- function(horizontal=TRUE, spacing=5, use.scrollwindow=FALSE, container=NULL, ..., toolkit=guiToolkit()) {

  if(is.character(toolkit))
    toolkit <- guiToolkit(toolkit)
  
  obj <- .ggroup(toolkit, horizontal, spacing=spacing, use.scrollwindow=use.scrollwindow, container, ...)

  check_return_class(obj, "GGroup")
  obj   
  
}

##' S3 generic whose methods are implemented in the toolkit packages
##'
##' @rdname ggroup
##' @export
.ggroup <- function(toolkit, horizontal=TRUE, spacing=5, use.scrollwindow=FALSE, container=NULL, ...) UseMethod(".ggroup")



##' \code{svalue<-} method for a ggroup
##'
##' The main property of the box container is the spacing. There are
##' generally two types of spacing: padding around border of the box
##' and spacing between each child that is packed in.
##' @param obj \code{ggroup} instance
##' @param index ignored
##' @param ... ignored
##' @param value integer amount of spacing
##' @export
##' @rdname ggroup
"svalue<-.GGroup" <- function(obj, index=TRUE,  ..., value) {
  NextMethod()
}



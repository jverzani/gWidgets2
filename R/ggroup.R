##' @include methods.R
NULL

##' Basic box container
##'
##' @param horizontal logical. If TRUE, left to right layout, otherwise top to bottom
##' @param spacing spacing aroud widget 
##' @param use.scrollwindow logical. If TRUE, places box container in scrollwindow. In this case, a size should be set.
##' @inheritParams gwidget
##' @return a GGroup instance.
##' @export
##' @rdname ggroup
##' @seealso \code{\link{gframe}} and \code{\link{gexpandgroup}}
##' @example inst/examples/ex-boxcontainers.R
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
##' The \code{svalue} method refers to the main property of the box
##' container, its spacing. There are generally two types of spacing:
##' padding around border of the box and spacing between each child
##' that is packed in. The spacing here is the between-child-component spacing.
##' The reference class method \code{set_borderwidth} can be used for the other.
##' @export
##' @rdname ggroup
##' @method svalue<- GGroup
##' @S3method svalue<- GGroup
"svalue<-.GGroup" <- function(obj, index=TRUE,  ..., value) {
  NextMethod()
}

##' Convenience constructor for vertical ggroup
##'
##' Avoids need to type \code{horizontal=FALSE}
##' @inheritParams ggroup
##' @return a GGroup instance with vertical packing.
##' @export
##' @rdname ggroup
gvbox <- function(spacing=5, use.scrollwindow=FALSE, container=NULL, ..., toolkit=guiToolkit())
  ggroup(horizontal=FALSE, spacing=spacing, use.scrollwindow=use.scrollwindow, container=container, ..., toolkit=toolkit)

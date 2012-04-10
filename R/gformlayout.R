##' @include methods.R
NULL





##' A form layout container
##'
##' This convenience container is basically a simpler form of
##' \code{glayout} to be used to layout two columns forms with a label
##' on the left.  The label can be passed in to the \code{add} method
##' of the container as is done with notebook labels
##' @param align alignment of label. Left justify or center balance. Leave as "default" for underlying toolkit default.
##' @param spacing spacing between columns
##' @inheritParams gwidget
##' @export
gformlayout <- function(
                        align=c("default", "left", "center"),
                        spacing=5,
                        container = NULL, ... ,
                        toolkit=guiToolkit()){

  
  obj <- .gformlayout(toolkit,
                      align=match.arg(align),
                      spacing=spacing,
                      container=container ,...
                      )
  check_return_class(obj, "GFormLayout")
  return(obj)
}


##' .gformlayout generic for toolkit dispatch
##'
##' @export
##' @rdname gformlayout
.gformlayout <- function(toolkit,
                         align="left",
                         spacing=5,
                         container = NULL,
                         ... )
  UseMethod( '.gformlayout' )

##' svalue method
##'
##' The \code{svalue} method for \code{GFormLayout} returns a list of values created by calling \code{svalue} on each child
##' @inheritParams svalue
##' @export
##' @rdname gformlayout
##' @method svalue GFormLayout
##' @S3method svalue GFormLayout
svalue.GButton <- function(obj, index=NULL, drop=NULL, ...)   NextMethod()

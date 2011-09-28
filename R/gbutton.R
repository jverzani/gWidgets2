##' @include methods.R
NULL

##' Basic button widget
##'
##' button widget
##' @param text label text. If text matches a stock icon name, that is used as well
##' @param handler optional handler. If given added through addHandlerChanged
##' @param action passed to handler through \code{action} component of first argument of handler. For buttons, this may also be a \code{GAction} instance.
##' @param container parent container (Optional for some toolkits, but not all).
##' @param ... passed to \code{add} method of parent container
##' @param toolkit toolkit instance 
##' @return a \code{GButton} instance. While this object has its own (reference) methods, one primarily interacts with it through S3 methods defined within the package.
##' @note removed the border option. This may be supported in the toolkit object 
##' @export
gbutton <- function(text="",   handler=NULL, action=NULL, container=NULL, ..., toolkit=guiToolkit()) {

  if(is.character(toolkit))
    toolkit <- guiToolkit(toolkit)

  deprecated_args <- list(border=c("Border argument has been deprecated.","See reference class method remove_border for availability"))
  check_deprecated(deprecated_args, ...)

  
  obj <- .gbutton(toolkit, text, handler, action, container, ...)

  check_return_class(obj, "GButton")
  obj
  
}

##' S3 generic whose methods are implemented in the toolkit packages
##'
##' @rdname gbutton
##' @export
.gbutton <- function(toolkit, text, handler, action, container, ...) UseMethod(".gbutton")


##' @include methods.R
NULL

##' constructor for checkbox widget
##'
##' A checkbox widget is used to toggle the state of a labeled boolean
##' variable. The main property of this widget is that state, not the
##' label. This variable may be proxied in the usual way -- with a box
##' that indicates or check if \code{TRUE} -- or through a toggle
##' button. 
##' @param text label text
##' @param checked is button selected
##' @param use.togglebutton Use a toggle button (shows depressed) not a check box
##' @param handler Callback called when toggle is changed.
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of container
##' @param toolkit toolkit
##' @export
##' @return Returns an object of class \code{GCheckbox}.
gcheckbox <- function(
                      text="", checked = FALSE, use.togglebutton=FALSE,
                      handler = NULL, action = NULL, container = NULL, ... ,
                      toolkit=guiToolkit()){

  ## text is just first value
  text <- as.character(text)[1]
  ## checked is logical
  checked <- as.logical(checked)[1]
  
  obj <- .gcheckbox (toolkit,
                     text=text, checked=checked,
                     use.togglebutton=use.togglebutton,
                     handler=handler, action=action, container=container, ...
                     )

  check_return_class(obj, "GCheckbox")
  obj
  
}


##' Generic for toolkit dispatch
##'
##' @export
##' @rdname gcheckbox
.gcheckbox <- function(toolkit,
                       text, checked = FALSE, use.togglebutton=FALSE, handler = NULL, action = NULL,
                       container = NULL, ... ) UseMethod( '.gcheckbox' )



##' svalue method
##'
##' Ensure value is logical
##' @export
##' @rdname gcheckbox
"svalue<-.GCheckbox" <- function(obj, index=NULL,  ...,value) {
  value <- as.logical(value)[1]
  NextMethod()
}


##' items assignment takes string
##'
##' @inheritParams base::Extract
##' @param value Value coerced to character, then only first element
##' used for checkbox label
##' @export
##' @rdname gcheckbox
"[<-.GCheckbox" <- function(x,i,j,...,value) {
  value <- as.character(value)[1]
  NextMethod()
}

##' @include gframe.R
NULL

##' Constructor of box container widget with disclosure trigger and label
##'
##' @export
##' @param text Label text
##' @param markup logical. Does text have markup. (Toolkit dependent)
##' @param horizontal horizontal (\code{TRUE}) or vertical packing.
##' @param handler handler called when toggled
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to parent's \code{add} method
##' @param toolkit toolkit
##' @return An object of class \code{GExpandGroup}. This (basically)
##' inherits from \code{GFrame} its methods and overrides:
##'
##' \enumerate{
##'
##' \item \code{visible<-} Logical. To specify if widget is open (\code{TRUE}) or closed.
##'
##' }
##'
##' 
gexpandgroup <- function(
                         text = "", markup = FALSE, horizontal=TRUE,
                         handler = NULL, action = NULL,
                         container = NULL, ... ,
                         toolkit=guiToolkit()){
  obj <- .gexpandgroup (toolkit,
                        text=text, markup=markup, horizontal=horizontal,
                        handler=handler, action=action, container=container ,...
                        )
  visible(obj) <- TRUE               # initial state

  check_return_class(obj, "GExpandGroup")
  obj   
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gexpandgroup
.gexpandgroup <- 
  function(toolkit,
           text = "", markup = FALSE,horizontal=TRUE,
           handler = NULL, action = NULL,
           container = NULL, ... )
  UseMethod( '.gexpandgroup' )



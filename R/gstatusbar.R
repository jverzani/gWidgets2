##' @include methods.R
NULL

##' constructor to add a status bar to main window
##'
##' @param text inital status text
##' @inheritParams gcontainer
##' @export
gstatusbar <- function(
                       text = "", container = NULL, ... ,
                       toolkit=guiToolkit()){
  obj <- .gstatusbar (toolkit,
                      text=text, container=container ,...
                      )
  check_return_class(obj, "GStatusBar")
  return(obj)
}


##' generic for toolkit dispatch
##' 
##' @rdname gstatusbar
##' @export
.gstatusbar <-  function(toolkit,
                         text = "", container = NULL, ... )
  UseMethod( '.gstatusbar' )

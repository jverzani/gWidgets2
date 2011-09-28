##' @include methods.R
NULL

##' constructor providing a widget for displaying a line in a GUI
##'
##' @export
gseparator <- function(
                       horizontal = TRUE, container = NULL, ... ,
                       toolkit=guiToolkit()){
  obj <- .gseparator (toolkit,
               horizontal=horizontal, container=container ,...
               )
  check_return_class(obj, "GSeparator")
  obj
  
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gseparator
.gseparator <- function(toolkit,
                    horizontal = TRUE, container = NULL, ... )
           UseMethod( '.gseparator' )

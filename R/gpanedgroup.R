##' @include methods.R
NULL


##' constructor for a two-paned container
##'
##' Holds two child widgets.
##' @param horizontal direction of layout
##' @export
##' @note There were arguments to specify the children at construction, but these have been removed.
gpanedgroup <- function(horizontal = TRUE,  container = NULL , ...,
                        toolkit=guiToolkit()){
  obj <- .gpanedgroup (toolkit,
                       horizontal=horizontal, 
                       container=container, ...
                )

  check_return_class(obj, "GNotebook")
  obj   
  
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gpanedgroup
.gpanedgroup <-  function(toolkit,
                          horizontal = TRUE, container = NULL, ... )
  UseMethod( '.gpanedgroup' )

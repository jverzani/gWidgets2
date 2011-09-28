##' @include methods.R
NULL

##' Constructor for a stack of widgets
##'
##' This widget is like a notebook -- it holds a stack of pages, but does not provide the tabs to work with. Most
##' methods are inherited from gnotebook's.
##' @export
gstackwidget <- function(container = NULL, ... ,
                      toolkit=guiToolkit()){

  obj <- .gstackwidget(toolkit,
                container=container ,...
                )
  check_return_class(obj, "GStackWidget")
  obj
  
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gstackwidget
.gstackwidget <-  function(toolkit,
                        container = NULL, ... )
           UseMethod( '.gstackwidget' )

## toolkit class should inherit from GNotebook

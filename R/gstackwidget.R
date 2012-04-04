##' @include methods.R
NULL

##' Constructor for a stack of widgets
##'
##' This widget is like a notebook -- it holds a stack of pages, but
##' does not provide the tabs to work with. Most methods are inherited
##' from gnotebook's.
##' @inheritParams gcontainer
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
## but just in case


##' Remove current page from notebook
##'
##' Dispose deletes the current page, not the entire notebook
##' object. To delete a specific page, a combination of
##' \code{svalue<-} and \code{dispose} may be used.
##' @export
##' @rdname gnotebook
##' @method dispose GStackWidget
##' @S3method dispose GStackWidget
dispose.GStackWidget <- function(obj, ...) {
  obj$remove_current_page()
}



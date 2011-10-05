##' @include methods.R
NULL

##' Constructor for a tabbed notebook container
##'
##' @note the button arguments of the gWidgets constructor are
##' removed. One passes the close button request to the \code{add}
##' method.
##' @param tab.pos integer. Position of tabs, 1 on bottom, 2 left, 3 top, 4 right. (If supported)
##' @export
gnotebook <- function(
                      tab.pos = 3, 
                      container = NULL, ... ,
                      toolkit=guiToolkit()){

  obj <- .gnotebook (toolkit,
              tab.pos=tab.pos, 
              container=container ,...
              )

  check_return_class(obj, "GNotebook")
  obj   
  
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gnotebook
.gnotebook <-  function(toolkit,
                        tab.pos = 3, 
                        container = NULL, ... )
  UseMethod( '.gnotebook' )

##' add method for notebooks
##'
##' Children added to notebooks need a label, a position and
##' optionally a close button (if supported). The arguments expand,
##' fill, anchor are not specified -- children expand and fill.
##' @param obj gnotebook object
##' @param child some child component to add
##' @param label character. Label text for tab
##' @param i integer. Position in 0..length(obj) to insert child. If empty at end, if 0 or less at beginning.
##' @param close.button logical Do we add a close button (toolkit dependent)
##' @param ... 
##' @return adds child
##' @export
##' @rdname gnotebook
add.GNotebook <- function(obj, child, label="", i=length(obj), close.button=FALSE, ...) {
  obj$add_child(child, label, i, close.button, ...)
}


##' Remove current page from notebook
##'
##' @export
##' @rdname gnotebook
dispose.GNotebook <- function(obj, ...) {
  obj$remove_current_page()
}


##' get tab names of notebook
##'
##' @export
##' @rdname gnotebook
"names.GNotebook" <- function(x) x$get_names()

##' set tab names for object
##'
##' @param x notebook object
##' @param value new label text
##' @export
##' @rdname gnotebook
"names<-.GNotebook" <- function(x, value) {
  x$set_names(value)
  x
}

##' add change handler
##'
##' the change handler for the notebook is called when the page changes. If possible, the new page number is passed back in the \code{page.no} component of 'h'
##' @export
##' @rdname gnotebook
addHandlerChanged <- function(obj, handler, action=NULL, ...) {
  NextMethod()
}

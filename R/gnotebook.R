##' @include methods.R
NULL

##' Constructor for a tabbed notebook container
##'
##' The tabbed notebook container allows one to hold many different
##' pages with a mechanism to switch between them. In \code{gWidgets2}
##' new pages are added through the \code{add} method. One passes in
##' the tab label through the extra \code{label} argument, or
##' subsequently through \code{names<-}.
##' 
##' @param tab.pos integer. Position of tabs, 1 on bottom, 2 left, 3 top, 4 right. (If supported)
##' @inheritParams gwidget
##' @note In \pkg{gWidgets2} the button arguments of the
##' \code{gWidgets} constructor are removed. One passes the close
##' button request to the \code{add} method.
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
##' @inheritParams add
##' @note To keep the signature the same as the generic, several arguments are passed in via ...:
##' 
##' \describe{
##' 
##' \item{label}{ A character. Label text for tab}
##' 
##' \item{i}{An integer in \code{0} to \code{length(obj)} indicating
##' the position to insert child. The new page is inserted to the
##' right of page  number \code{i}. When \code{i=0}, the page appears
##' at the front, when \code{i} is not specified it appears at the
##' end.
##' }
##' 
##' \item{close.button}{A logical. If \code{TRUE} -- and the toolkit
##' supports it -- the page tab will include a close button.
##' }
##' }
##' @return none. called for its side effect.
##' @export
##' @rdname gnotebook
##' @method add GNotebook
##' @S3method add GNotebook
add.GNotebook <- function(obj, child, expand, fill, anchor, ...) {
  ## process passed in args
  args <- list(...)
  label <- getWithDefault(args$label, "")
  i <- getWithDefault(args$i, length(obj))
  close.button <- getWithDefault(args$close.button, FALSE)
  
  obj$add_child(child, label, i, close.button, ...)
}


##' Remove current page from notebook
##'
##' Dispose deletes the current page, not the entire notebook
##' object. To delete a specific page, a combination of
##' \code{svalue<-} and \code{dispose} may be used.
##' @export
##' @rdname gnotebook
##' @method dispose GNotebook
##' @S3method dispose GNotebook
dispose.GNotebook <- function(obj, ...) {
  obj$remove_current_page()
}


##' get tab names of notebook
##'
##' The \code{names} of a notebook are the page tab labels. These may
##' be retrieved and set through the \code{names} method.
##' @param x notebook object
##' @export
##' @rdname gnotebook
##' @method names GNotebook
##' @S3method names GNotebook
"names.GNotebook" <- function(x) x$get_names()


##' add change handler
##'
##' the change handler for the notebook is called when the page
##' changes. If possible, the new page number is passed back in the
##' \code{page.no} component of 'h'
##' @export
##' @rdname gnotebook
##' @method addHandlerChanged GNotebook
##' @S3method addHandlerChanged GNotebook
addHandlerChanged.GNotebook <- function(obj, handler, action=NULL, ...) {
  NextMethod()
}

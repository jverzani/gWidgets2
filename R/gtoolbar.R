##' @include methods.R
NULL


##' A toolbar constructor
##'
##' @param toolbar.list list. A one-level list of \code{gaction} items, \code{gseparator} items or possibly other widgets. In the latter cases the \code{container} argument is not specified prior. (XXX Need to work this out with gWidgetstcltk)
##' @param style style for icon or text.
##' @param container a GWindow instance
##' @param ... ignored
##' @param toolkit toolkit
##' @export
gtoolbar <- function(
                     toolbar.list=list(),
                     style = c("both", "icons", "text", "both-horiz"),
                     container = NULL,
                     ... ,
                     toolkit=guiToolkit()){

  deprecated_args <- list(toolbarlist="Use toolbar.list instead",
                          action="No action argument, parameterize gaction objects individually")
  check_deprecated(deprecated_args, ...)
    
  obj <- .gtoolbar (toolkit,
                    toolbar.list=toolbar.list,
                    style=match.arg(style),
                    container=container ,...
                    )
  check_return_class(obj, "GToolBar")
  return(obj)
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gtoolbar
.gtoolbar <- function(toolkit,
                      toolbar.list=list(),
                      style = c("both", "icons", "text", "both-horiz"),
                      container = NULL,
                      ... )
           UseMethod( '.gtoolbar' )


##' add toolbar items to toolbar
##'
##' @inheritParams add
##' @child list. a toolbar list (one-level named list of \code{gaction} or \code{gseparator} objects.)
##' @export
##' @rdname gtoolbar
add.GToolBar <- function(obj, child, expand=FALSE, fill=NULL, anchor=NULL, ...) {
  dispatcher <- function(obj, child) UseMethod("dispatcher")
  dispatcher.GToolBar <- function(child, obj) obj$add_toolbar_items(svalue(child))
  dispatcher.list <- function(obj, child) obj$add_toolbar_items(child)
  dispatcher(child, obj)
}




##' "svalue<-" method
##'
##' for a toolbar, \code{svalue<-} replaces the toolbar items with new ones specified by value.
##' @inheritParams svalue
##' @value For a toolbar, a list defining the new toolbar.
##' @export
##' @rdname svalue
"svalue<-.GToolBar" <- function(obj, index=NULL, ..., value) NextMethod()

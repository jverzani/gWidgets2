##' @include methods.R
NULL


##' A toolbar constructor
##'
##' @export
gtoolbar <- function(
                     toolbar.list=list(),
                     style = c("both", "icons", "text", "both-horiz"),
                     container = NULL, ... ,
                     toolkit=guiToolkit()){

  deprecated_args <- list(toolbarlist="Use toolbar.list instead",
                          action="No action argument, parameterize gaction objects individually")
  check_deprecated(deprecated_args, ...)
    
  obj <- .gtoolbar (toolkit,
                    toolbar.list=toolbar.list, style=match.arg(style),
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
                      toolbarlist=list(),
                      style = c("both", "icons", "text", "both-horiz"),
                      container = NULL,
                      ... )
           UseMethod( '.gtoolbar' )


##' add toolbar items to toolbar
##' 
##' @export
##' @rdname gtoolbar
add.GToolBar <- function(obj, child, expand=FALSE, fill=NULL, anchor=NULL, ...) {
  dispatcher <- function(obj, child) UseMethod("dispatcher")
  dispatcher.GToolBar <- function(child, obj) obj$add_toolbar_items(svalue(child))
  dispatcher.list <- function(obj, child) obj$add_toolbar_items(child)
  dispatcher(child, obj)
}




##' svalue method
##'
##' for a list, svalue replaces the toolbar items with new ones specified by value
##' @export
##' @rdname svalue
"svalue<-.GToolBar" <- function(obj, index=NULL, ..., value) nextMethod()

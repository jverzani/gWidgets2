##' @include methods.R
NULL

##' Constructor for checkbox group. A linked group of checkboxes, but not exclusive.
##'
##' @export
##' @param items checkbox labels
##' @param checked logical. Are values checked
##' @param horizontal logical. If true displayed horizontally, else vertically
##' @param use.table logical. If supported, and \code{TRUE} then use a table widget with scrollbars
##' @param handler Handler called when state toggles
##' @param action passed to handler when called
##' @param container parent container
##' @param ... passed to \code{add} method of parent
##' @param toolkit toolkit
##' @return Returns an object of class \code{GCheckboxGroup} for which the following methods are overridden:
##' %
##' \itemize{
##'
##' \item{ \code{svalue} Return the selected values or an empty
##' character vector. If \code{index=TRUE}, returns indices of
##' selected values.}
##' 
##' \item{ \code{svalue<-} Set the selected values one of three ways:
##' by label name, by a logical variable indicating which are selected
##' (if ambigous, logical wins), if \code{index=TRUE} by the indices
##' to select.}
##' 
##' \item{ \code{[} returns labels}
##' 
##' \item{ \code{[<-} set the label values. Should be able to shorten
##' or lengthen list}
##' 
##' }
gcheckboxgroup <- function(
                           items, checked = FALSE, horizontal = FALSE,
                           use.table=FALSE, handler = NULL,
                           action = NULL, container = NULL, ... ,
                           toolkit=guiToolkit()){

  if(missing(items))
    items <- character(0)
  horizontal <- as.logical(horizontal)
  
  
  obj <- .gcheckboxgroup (toolkit,
                          items=items, checked=checked, horizontal=horizontal, use.table=use.table,
                          handler=handler, action=action, container=container, ...
                          )

  check_return_class(obj,  "GCheckboxGroup")
  obj
  
 
}


##' generic for toolkit dispatch
##' 
##' @rdname gcheckboxgroup
##' @export
.gcheckboxgroup <- function(toolkit,
                            items, checked = FALSE, horizontal = FALSE,
                            handler = NULL, action = NULL,
                            container = NULL, ... ) UseMethod( '.gcheckboxgroup' )



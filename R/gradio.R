##' @include methods.R
NULL


##' Constructor for radio button widget
##' @param items items to select from
##' @param selected index of initially selected item
##' @param horizontal layout direction
##' @param handler handler for toggle event
##' @param action action
##' @param container parent container
##' @param ... passed to add method of parent containers
##' @param toolkit toolkit
##' @export
##' @rdname gradio
gradio <- function(items,selected=1, horizontal=FALSE, handler=NULL,
                   action=NULL, container=NULL, ...,
                   toolkit=guiToolkit()) {
  
  ## check input
  if(length(x <- unique(items) ) != length(items))
    message("Using unique items for selection values")
  
  obj <- .gradio(toolkit, x, selected, horizontal, handler, action, container,...)
  check_return_class(obj, "GRadio")
  obj
}

##' Generic for method dispatch
##'
##' @export
##' @rdname gradio
.gradio <- function(toolkit,
                    items, selected=1, horizontal=FALSE, handler=NULL, action=NULL,
                    container=NULL,
                    ...) UseMethod(".gradio")

##' assign items for gradio
##'
##' Check for repeated itesm
##' @param x object
##' @param i ignored
##' @param j ignored
##' @param ... ignored
##' @param value vector of items to select from
##' @export
##' @rdname gradio
"[<-.GRadio" <- function(x, i, j, ..., value) {
  ## check input
  if(length(value) != length(value <- unique(value)))
    message("Using unique values for selection values")
  NextMethod()
}

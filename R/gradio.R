##' @include methods.R
NULL


##' Constructor for radio button widget
##'
##' The radio button widget shows 2 or more items forcing the user to select one.
##' @param items items to select from
##' @param selected index of initially selected item
##' @param horizontal layout direction
##' @inheritParams gwidget
##' @export
##' @rdname gradio
##' @example inst/examples/ex-selectionwidgets.R
gradio <- function(items,selected=1, horizontal=FALSE,
                   handler=NULL, action=NULL,
                   container=NULL, ...,
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



##' svalue method
##'
##' For a radio button group, with \code{svalue} the value can be referred to by index or
##' label. Similarly with setting of a value
##' @inheritParams gWidgets2::svalue
##' @export
##' @rdname gradio
##' @S3method svalue GRadio
##' @method svalue GRadio
svalue.GRadio <- function(obj, index=NULL, drop=TRUE, ...) NextMethod()


##' svalue<- method
##'
##' For a radio button group, with \code{svalue} the value can be referred to by index or
##' label. Similarly with setting of a value
##' @inheritParams gWidgets2::svalue
##' @export
##' @rdname gradio
##' @S3method svalue<- GRadio
##' @method svalue<- GRadio
"svalue<-.GRadio" <- function(obj, index=NULL, drop=TRUE, ..., value) {
  if(!is.null(index) && index) {
    value <- as.integer(value)[1]
    if(value < 1 || value > length(obj)) warning(gettext("Index is out of range"))
  }
  if(is.null(index) || !index) {
    if(! value %in% obj[])
      warning(gettext("Value specified is not one of the items"))
  }

  NextMethod()
}


##' assign items for gradio
##'
##' Check for repeated items before passing on to \code{set_items}
##' @export
##' @rdname gradio
##' @method [<- GRadio
##' @S3method [<- GRadio
"[<-.GRadio" <- function(x, i, j, ..., value) {
  ## check input
  if(length(value) != length(value <- unique(value)))
    message("Using unique values for selection values")
  NextMethod()
}

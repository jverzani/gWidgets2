##' @include S3-methods.R
NULL

## generic methods and definitions

##' svalue
##'
##' This returns the
##' "selected" value in a widget. Selection varies from widget to
##' widget, but should generally is what can be added to the
##' widget by mouse click or typing. For some widgets, the extra
##' argument ‘index=TRUE’ will return the index of the selected
##' value, not the value. For some widget, the argument ‘drop’ is
##' given to either prevent or encourage dropping of information.
##' @param obj object of method call
##' @param index NULL or logical. If \code{TRUE} and widget supports it an index, instead of a value will be returned.
##' @param drop NULL or logical. If widget supports it, drop will work as it does in a data frame or perhaps someother means.
##' @param ... passed on to call
##' @rdname svalue
##' @export
svalue <- function(obj, index=FALSE, drop=NULL, ...) UseMethod("svalue")

##' default svalue instance
##'
##' Calls coerce_with when available
##' @export
##' @rdname svalue
svalue.default <- function(obj, index=NULL, drop=NULL, ...) {
  if(!isExtant(obj)) {
    return()
  }
  if(getWithDefault(index, FALSE)) {
    val <- obj$get_index(drop=drop, ...)
  } else {
    val <- obj$get_value(drop=drop, ...)
    if(exists("coerce_with", obj) &&
       !is(obj$coerce_with, "uninitializedField") &&
       !is.null(obj$coerce_with)) {
      if(is.character(obj$coerce_with))
        obj$coerce_with <- get(obj$coerce_with, inherits=TRUE)
      val <- obj$coerce_with(val)
    }
  }
  val
}

##' svalue<-
##'
##' This method sets the main property of the widget.
##' @rdname svalue
##' @export
"svalue<-" <- function(obj, index=TRUE, drop=TRUE, ..., value) UseMethod("svalue<-")

##' Base S3 method
##'
##' @rdname svalue
##' @export
"svalue<-.default" <- function(obj, index=NULL, ..., value) {
  if(!isExtant(obj)) {
    return(obj)
  }

  if(getWithDefault(index, FALSE))
    obj$set_index(value, ...)
  else
    obj$set_value(value, ...)
  obj
}

##' enabled
##'
##' A widget is enabled if it is sensitive to user input
##' @param obj object
##' @export
##' @return logical indicating if widget is enabled
##' @rdname enabled
enabled <- function(obj) UseMethod("enabled")

##' base S3 method for enabled.
##'
##' @export
##' @rdname enabled
enabled.default <- function(obj) {
  if(isExtant(obj))
    obj$get_enabled()
}
##' Set whether widget is enabled or not
##'
##' @param value logical
##' @return if \code{value} is logical and \code{FALSE} widget will be insensitive to user input and rendered in a muted state.
##' @export
##' @usage enabled(obj) <- value
##' @rdname enabled
"enabled<-" <- function(obj, value) UseMethod("enabled<-")

##' S3 method for setting enabled property
##'
##' @export
##' @rdname enabled
"enabled<-.default" <- function(obj, value) {
  if(isExtant(obj))
    obj$set_enabled(as.logical(value))
  obj
}

##' Controls whether widget is visible or not
##'
##' For most -- but not all -- widgets, a widget is visible if it is
##' shown. For others, parts of the widget may be controlled by
##' visible. If the former state is desired, simply place widget into
##' a box container.
##' @param obj object
##' @export
##' @rdname visible
visible <- function(obj, ...) UseMethod("visible")

##' Basic S3 method
##'
##' @export
##' @rdname visible
visible.default <- function(obj, ...) {
  if(isExtant(obj))
    obj$get_visible()
}

##' Set visibility of an object
##'
##' @param value logical. Set visible state.
##' @export
##' @usage visible(obj) <- value
##' @rdname visible
"visible<-" <- function(obj, value) UseMethod("visible<-")

##' Basic S3 method for visible
##'
##' @export
##' @rdname visible
"visible<-.default" <- function(obj, value) {
  if(isExtant(obj))
    obj$set_visible(as.logical(value))
  obj
}

##' Does widget have focus
##'
##' a widget has focus if it will receive input events
##' @param obj object
##' @export
##' @rdname focus
focus <- function(obj) UseMethod("focus")

##' Basic S3 method
##'
##' @export
##' @rdname focus
focus.default <- function(obj) {
  if(isExtant(obj))
    obj$get_focus()
}

##' Set focus onto object. 
##'
##' For some widgets, this sets user focus (e.g. gedit gets focus for
##' typing). For others, settig the focus calls the raise
##' methods. (gor gwindow, it will raise the window)
##' @param value logical. Set focus state.
##' @export
##' @usage focus(obj) <- value
##' @rdname focus
"focus<-" <- function(obj, value) UseMethod("focus<-")

##' Basic S3 method for focus
##'
##' @export
##' @rdname focus
"focus<-.default" <- function(obj, value) {
  if(isExtant(obj))
    obj$set_focus(as.logical(value))
  obj
}


##' Controls whether widget is editable or not
##'
##' Some widgets may be editable. If possible, the setter method can
##' be used to toggle the state. This method indicates the state.
##' @param obj object
##' @export
##' @rdname editable
editable <- function(obj) UseMethod("editable")

##' Basic S3 method
##'
##' @export
##' @rdname editable
editable.default <- function(obj) {
  if(isExtant(obj))
    obj$get_editable()
}
##' Set whether an object can be edited
##'
##' @param value logical. Set editable state.
##' @export
##' @usage editable(obj) <- value
##' @rdname editable
"editable<-" <- function(obj, value) UseMethod("editable<-")

##' Basic S3 method for editable
##'
##' @export
##' @rdname editable
"editable<-.default" <- function(obj, value) {
  if(isExtant(obj))
    obj$set_editable(as.logical(value))
  obj
}

##' Returns font specification for widget, if available
##'
##' @param obj object
##' @export
##' @rdname font
font <- function(obj) UseMethod("font")

##' Basic S3 method for font
##'
##' @export
##' @rdname font
font.default <- function(obj) {
  if(isExtant(obj))
    obj$get_font()
}
##' Set font for a widget
##'
##' @param value The font specification is given in terms of a named vector or list where the names indicate a font attribute and the value a reasonable choice:
##' \describe{
##' \item{weight}{c("light", "normal", "medium", "bold", "heavy")}
##' \item{style}{c("normal", "oblique", "italic")}
##' \item{family}{c("sans", "helvetica", "times", "monospace")}
##' \item{size}{an integer, say c(6,8,10,11,12,14,16,18,20, 24,36,72)}
##' \item{color (or foreground)}{One of colors()}
##' \item{background}{One of colors()}
##' \item{scale}{c("xx-large", "x-large",  "large" ,   "medium",   "small",    "x-small",  "xx-small")}
##' }
##' These are from Gtk's font specs, which though fairly standard, may not be totally supported in the other toolkits.
##' @export
##' @usage font(obj) <- value
##' @rdname font
"font<-" <- function(obj, value) UseMethod("font<-")

##' Basic S3 method for setting font
##'
##' @export
##' @rdname font
"font<-.default" <- function(obj, value) {
  if(isExtant(obj))
    obj$set_font(value)
  obj
}

##' get a persistent attribute for an object
##'
##' @param obj object
##' @param key character. Values are stored by key. If missing, all keys are returned.
##' @export
##' @rdname tag
tag <- function(obj, key) UseMethod("tag")

##' Basic S3 method
##'
##' @export
##' @rdname tag
tag.default <- function(obj, key) {
  if(isExtant(obj))
    obj$get_attr(key)
}

##' set a persistent attribute for an object
##'
##' Unlike \code{attr<-}, this method (essentially) stores the
##' attribute in a reference to the object, not a copy. As such it can
##' be used within function call (handlers) to assign values outside
##' the scope of the function call.
##' @param value to assign to key
##' @export
##' @usage tag(obj, key) <- value
##' @rdname tag
"tag<-" <- function(obj, key, value) UseMethod("tag<-")

##' Basic S3 method
##'
##' @export
##' @rdname tag
"tag<-.default" <- function(obj, key, value) {
  if(isExtant(obj))
    obj$set_attr(key, value)
  obj
}

## XXX add others  size, size<-, 

##' Return size (width and height) of widget
##'
##' @param obj object
##' @rdname size
##' @export
size <- function(obj) UseMethod("size")

##' S3 method for size
##'
##' @export
##' @rdname size
size.default <- function(obj) {
  if(isExtant(obj))
    obj$get_size()
}

##' Set size of object (width, height)
##'
##' The size is specified in pixels (integers). Some toolkits allow -1 as a default, but not all.
##' @export
##' @usage size(obj) <- value
##' @rdname size
"size<-" <- function(obj, value) UseMethod("size<-")

##' S3 method for size
##'
##' @export
##' @rdname size
"size<-.default" <- function(obj, value) {
  if(isExtant(obj))
    obj$set_size(value)
  obj
}

##' Get a tooltip for the widget
##'
##' @param obj object
##' @export
##' @rdname tooltip
"tooltip" <- function(obj) UseMethod("tooltip")

##' Basic S3 method for tooltip<-
##'
##' @export
##' @rdname tooltip
"tooltip<-.default" <- function(obj) {
  if(isExtant(obj))
    obj$get_tooltip()
}

##' Set a tooltip for the widget
##'
##' @param obj object
##' @param value character tooltip value
##' @export
##' @usage tooltip(obj) <- value
##' @rdname tooltip
"tooltip<-" <- function(obj, value) UseMethod("tooltip<-")

##' Basic S3 method for tooltip<-
##'
##' @export
##' @rdname tooltip
"tooltip<-.default" <- function(obj, value) {
  if(isExtant(obj))
    obj$set_tooltip(paste(value, collapse="\n"))
  obj
}


##' Undo past action. 
##'
##' Some widgets support undo actions
##' @export
##' @rdname undo
undo <- function(obj, ...) UseMethod("undo")

##' S3 method. 
##'
##' @export
##' @rdname undo
undo.GComponent <- function(obj, ...) {
  if(isExtant(obj))
    obj$undo(...)
}



##' Redo past action. 
##'
##' Some widgets support redo actions
##' @export
##' @rdname redo
redo <- function(obj, ...) UseMethod("redo")

##' S3 method. 
##'
##' @export
##' @rdname redo
redo.GComponent <- function(obj, ...) {
  if(isExtant(obj))
    obj$redo(...)
}

##' Check if widget is extant.
##'
##' Widgets can be destroyed, but their R object is still present. This is FALSE in that case.
##' @param obj object
##' @export
##' @rdname isExtant
"isExtant" <- function(obj) UseMethod("isExtant")

##' Basic S3 method for isExtant
##'
##' @export
##' @rdname tooltip
"isExtant.default" <- function(obj) {
  ret <- try(obj$is_extant(), silent=TRUE)
  if(is(ret, "try-error"))
    FALSE
  else
    ret
}


## container methods

##' Add a child object to parent container
##'
##' Add packs in child objects into parent box containers
##' @param obj parent object
##' @param child child widget
##' @param expand NULL or logical. XXX
##' @param fill NULL or logical. XXX
##' @param anchor NULL or logical. XXX
##' @param ... passed on
##' @export
##' @rdname add
add <- function(obj, child, expand=FALSE, fill=NULL, anchor=NULL, ...) UseMethod("add")

##' Basic S3 method for add
##'
##' @export
##' @rdname add
add.default <- function(obj, child, expand=FALSE, fill=NULL, anchor=NULL, ...) {
  if(!isExtant(obj))  return()

  ## second dispatch based on type of child
  .add <- function(child, obj, ...) UseMethod(".add")  
  .add.GMenu <- function(child, obj, ...) {
    stop("Parent must be gwindow instance to add a menu")
  }
  .add.GToolBar <- function(child, obj, ...) {
    stop("Parent must be gwindow instance to add a toolbar")
  }
  .add.GStatusbar <- function(child, obj, ...) {
    stop("Parent must be gwindow instance to add a statusbar")
  }
  .add.default <- function(child, obj, expand, fill, anchor, ...) obj$add_child(child, expand=expand, fill=fill, anchor=anchor, ...)

  .add(child, obj, expand=expand, fill=fill, anchor=anchor, ...)
  
}



##' Delete child object from parent
##'
##' Delete may or may note remove a child. This is toolkit
##' specific. It may also be tied up with garbage collection. To avoid
##' that, keep a reference to the child object before deleting.
##' @export
##' @rdname add
delete <- function(obj, child) UseMethod("delete")

##' Basic S3 method for add
##'
##' @export
##' @rdname add
delete.GContainer <- function(obj, child) {
  if(isExtant(obj))
    obj$remove_child(child)
}

##' Dispose of object
##'
##' Dispose of object, primarily a window
##' @export
##' @rdname dispose
dispose <- function(obj, ...) UseMethod("dispose")

##' main dispose method. Calls dispose for GWindow
##'
##' @export
##' @rdname dispose
dispose.GComponent <- function(obj, ...) {
  if(isExtant(obj))
    dispose(getTopLevel(obj))
}

## XXX dispose.GNotebook removes page

##' Get underlying toolkit widget
##'
##' At times a user may wish to access the underlying toolkit
##' widget. Although this is not cross-platform, one often has access
##' to many more methods of the object, than through those provided by
##' gWidgets.
##' @param obj object
##' @export
##' @rdname getToolkitWidget
getToolkitWidget <- function(obj) UseMethod("getToolkitWidget")

##' Basic S3 method 
##'
##' @export
##' @rdname getToolkitWidget
getToolkitWidget.default <- function(obj) getWidget(obj)

##' Get underlying toolkit widget from widget slot. Used internally
##'
##' @export
##' @rdname getToolkitWidget
getWidget <- function(obj) UseMethod("getWidget")

##' method for getWidget
##'
##' @rdname getToolkitWidget
##' @export
getWidget.GComponent <- function(obj) getWidget(obj$widget)
## implement getWidget.RGtkObject <- function(obj) obj say

##' Get underlying toolkit object from block slot
##'
##' @rdname getToolkitWidget
##' @export
getBlock <- function(obj) UseMethod("getBlock")

##' S3 method for getBlock generic
##'
##' @rdname getToolkitWidget
##' @export
getBlock.GComponent <- function(obj) getBlock(obj$block)


##' S3 method for getBlock generic
##'
##' For GWindow, the block is NULL
##' @rdname getToolkitWidget
##' @export
getBlock.GWindow <- function(obj) obj$widget
             
##' Get toplevel window containing object
##'
##' @param obj object
##' @export
##' @rdname getToolkitWidget
getTopLevel <- function(obj) UseMethod("getTopLevel")

##' getTopLevel method for components
##'
##' @export
##' @rdname getToolkitWidget
getTopLevel.GComponent <- function(obj) {
  if(!is(obj, "GComponent"))
    stop("Must call getTopLevel with a GComponent object")
  if(is(obj$parent, "uninitializedField") || is.null(obj$parent))
    return(obj)
  else
    getTopLevel(obj$parent)
}

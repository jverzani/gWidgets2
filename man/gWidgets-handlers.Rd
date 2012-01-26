\name{addHandler}
\alias{add3rdmousePopupMenu}
\alias{add3rdmousePopupMenu.default}
\alias{addDragMotion}
\alias{addDragMotion.default}
\alias{addDropSource}
\alias{addDropSource.default}
\alias{addDropTarget}
\alias{addDropTarget.default}
\alias{addHandler}
\alias{addHandler.default}
\alias{addHandlerBlur}
\alias{addHandlerBlur.default}
\alias{addHandlerChanged}
\alias{addHandlerChanged.default}
\alias{addHandlerClicked}
\alias{addHandlerClicked.default}
\alias{addHandlerColumnclicked}
\alias{addHandlerColumnclicked.default}
\alias{addHandlerColumnDoubleclicked}
\alias{addHandlerColumnDoubleclicked.default}
\alias{addHandlerColumnRightclicked}
\alias{addHandlerColumnRightclicked.default}
\alias{addHandlerDestroy}
\alias{addHandlerDestroy.default}
\alias{addHandlerDoubleclick}
\alias{addHandlerDoubleclick.default}
\alias{addHandlerExpose}
\alias{addHandlerExpose.default}
\alias{addHandlerFocus}
\alias{addHandlerFocus.default}
\alias{addHandlerIdle}
\alias{addHandlerKeystroke}
\alias{addHandlerKeystroke.default}
\alias{addHandlerMouseMotion}
\alias{addHandlerMouseMotion.default}
\alias{addHandlerRightclick}
\alias{addHandlerRightclick.default}
\alias{addHandlerSelect}
\alias{addHandlerSelect.default}
\alias{addHandlerUnrealize}
\alias{addHandlerUnrealize.default}
\alias{addPopupMenu}
\alias{addPopupMenu.default}
\alias{blockHandler}
\alias{blockHandler.default}
\alias{blockHandlers}
\alias{blockHandlers.default}
\alias{removeHandler}
\alias{removeHandler.default}
\alias{unblockHandler}
\alias{unblockHandler.default}
\alias{unblockHandlers}
\alias{unblockHandlers.default}
\title{Generic method to add a handler passing a signal}
\usage{
  addHandler(obj, signal, handler, action = NULL, ...)

  \method{addHandler}{default} (obj, signal, handler,
  action = NULL, ...)

  addHandlerChanged(obj, handler, action = NULL, ...)

  \method{addHandlerChanged}{default} (obj, handler, action
  = NULL, ...)

  addHandlerClicked(obj, handler, action = NULL, ...)

  \method{addHandlerClicked}{default} (obj, handler, action
  = NULL, ...)

  addHandlerDoubleclick(obj, handler, action = NULL, ...)

  \method{addHandlerDoubleclick}{default} (obj, handler,
  action = NULL, ...)

  addHandlerRightclick(obj, handler, action = NULL, ...)

  \method{addHandlerRightclick}{default} (obj, handler,
  action = NULL, ...)

  addHandlerColumnclicked(obj, handler, action = NULL, ...)

  \method{addHandlerColumnclicked}{default} (obj, handler,
  action = NULL, ...)

  addHandlerColumnDoubleclicked(obj, handler, action =
  NULL, ...)

  \method{addHandlerColumnDoubleclicked}{default} (obj,
  handler, action = NULL, ...)

  addHandlerColumnRightclicked(obj, handler, action = NULL,
  ...)

  \method{addHandlerColumnRightclicked}{default} (obj,
  handler, action = NULL, ...)

  addHandlerSelect(obj, handler, action = NULL, ...)

  \method{addHandlerSelect}{default} (obj, handler, action
  = NULL, ...)

  addHandlerFocus(obj, handler, action = NULL, ...)

  \method{addHandlerFocus}{default} (obj, handler, action =
  NULL, ...)

  addHandlerBlur(obj, handler, action = NULL, ...)

  \method{addHandlerBlur}{default} (obj, handler, action =
  NULL, ...)

  addHandlerDestroy(obj, handler, action = NULL, ...)

  \method{addHandlerDestroy}{default} (obj, handler, action
  = NULL, ...)

  addHandlerUnrealize(obj, handler, action = NULL, ...)

  \method{addHandlerUnrealize}{default} (obj, handler,
  action = NULL, ...)

  addHandlerExpose(obj, handler, action = NULL, ...)

  \method{addHandlerExpose}{default} (obj, handler, action
  = NULL, ...)

  addHandlerKeystroke(obj, handler, action = NULL, ...)

  \method{addHandlerKeystroke}{default} (obj, handler,
  action = NULL, ...)

  addHandlerMouseMotion(obj, handler, action = NULL, ...)

  \method{addHandlerMouseMotion}{default} (obj, handler,
  action = NULL, ...)

  addHandlerIdle(...)

  addPopupMenu(obj, menulist, action = NULL, ...)

  \method{addPopupMenu}{default} (obj, menulist, action =
  NULL, ...)

  add3rdmousePopupMenu(obj, menulist, action = NULL, ...)

  \method{add3rdmousePopupMenu}{default} (obj, menulist,
  action = NULL, ...)

  addDropSource(obj, handler, action = NULL, data.type =
  c("text", "object"), ...)

  \method{addDropSource}{default} (obj, handler, action =
  NULL, data.type = c("text", "object"), ...)

  addDropTarget(obj, handler, action = NULL, ...)

  \method{addDropTarget}{default} (obj, handler, action =
  NULL, ...)

  addDragMotion(obj, handler, action = NULL, ...)

  \method{addDragMotion}{default} (obj, handler, action =
  NULL, ...)

  blockHandlers(obj, ...)

  \method{blockHandlers}{default} (obj, ...)

  blockHandler(obj, ID, ...)

  \method{blockHandler}{default} (obj, ID, ...)

  unblockHandlers(obj, ...)

  \method{unblockHandlers}{default} (obj, ...)

  unblockHandler(obj, ID, ...)

  \method{unblockHandler}{default} (obj, ID, ...)

  removeHandler(obj, ID, ...)

  \method{removeHandler}{default} (obj, ID, ...)
}
\arguments{
  \item{obj}{object receiving event and emitting a signal
  to the handler}

  \item{signal}{toolkit signal, e.g. "clicked"}

  \item{handler}{handler to assign when signal is emitted.
  A handler is a function, its first argument should expect
  a list with components \code{obj} containing a reference
  to the object and \code{action}. Some handlers are passed
  additional values.}

  \item{action}{passed to handler to parameterize call.}

  \item{...}{passed along}

  \item{menulist}{a list of \code{gaction} items}

  \item{data.type}{Type of data returned. It is either text
  or an object}

  \item{ID}{returned by addHandler. If missing will try to
  block handler passed to constructor}
}
\value{
  a handler ID which can be used to block/unblock or remove
  the handler

  NULL
}
\description{
  A GUI is made interactive by assigning handlers to
  user-generated events, such as a mouse click, change of
  widget state, or keyboard press. In \pkg{gWidgets2}
  handlers are assigned through the \code{addHandlerXXX}
  methods. The handlers are functions whose first argument
  should expect a list with components \code{obj} (to pass
  in the receiver object) and \code{action} (to pass in any
  user-supplied value to the \code{action} argument). Some
  handlers add other components, such as mouse position
  information on a click, or key information on a keyboard
  event.  Although this method is basically the workhorse
  to add a handler to response to a signal, it generally
  isn't called directly, as its use is not cross toolkit.
  Rather, if possible, one should use the
  \code{addHandlerXXX} methods to add a handler. These
  dispatch do this (basically) but do so in a toolkit
  independent manner. This call (and the others) returns a
  handler ID which may be used for some toolkitst later on
  to remove, block or unblock the call. All handlers for a
  widget may be blocked or unblocked via
  \code{blockHandlers} and \code{unblockHandlers}.

  Default S3 method

  The "changed" event is also the one that a handler passed
  to the constructor is called on. This is a real generic
  function, in that each widget has one, but it is
  interpreted quite differently for each.

  Default S3 method

  Add handler for clicked event

  Default S3 method

  Add handler for double click event

  Default S3 method

  Add handler for right click event

  Default S3 method

  Add handler for column click event

  Default S3 method

  Add handler for column double click event

  Default S3 method

  Add handler for column right click event

  Default S3 method

  Add a handler to the a "select" event

  Default S3 method

  Add handler for focus in event

  Default S3 method

  Add handler for blur, or focus-out, event

  Default S3 method

  Add handler for destroy event

  Default S3 method

  For gwindow objects this handler is called before the
  window is closed. If this handler returns \code{TRUE} the
  window will be closed, if \code{FALSE} the window will
  not be closed.

  Default S3 method

  Add handler for expose event (when a widget is exposed,
  say it had been covered)

  Default S3 method

  The "h" argument has components \code{key} for the key
  and possible \code{modifier} for the modifier.

  Default S3 method

  Add handler for mousemotion events

  Default S3 method

  deprecated. See \code{gtimer}.

  Add a "popup" menu to the widget

  S3 method for popup menu

  Add a 3rd-mouse "popup" menu to the widget

  S3 method for popup menu

  The handler should return the value to pass via drag and
  drop. It will appear as the \code{dropdata} component of
  the list passed in as the first argument of the handler

  Default S3 method

  The handler is called on the drop event. The component
  \code{dropdata} passes in the value being transferred by
  dragging.

  Default S3 method

  When a drag event crosses over the object the handler is
  called.

  Default S3 method

  Block all handlers for an object. Removed via
  unblockHandlers.

  S3 method to block all handlers

  Block a handler

  S3 method to block handler

  The block is a counter that gets decremented. If more
  blockHandlers call are made than unblockHandlers, the
  handlers will still be blocked.

  S3 method to block handler

  method call to unblock a blocked handler

  S3 method to block handler

  method call to unblock a remove permanently handler

  S3 method to remove handler
}
\note{
  This method is not toolkit independent, as the signal
  value depends on the toolkit

  For the gWidgets2Qt package one can not block, unblock or
  remove a single handler, but rather must do all the
  objects handlers at once. Specify no ID in this case.
}
\seealso{
  \code{\link{blockHandlers}},
  \code{\link{unblockHandlers}},
  \code{\link{blockHandler}}, \code{\link{unblockHandler}},
  and \code{\link{removeHandler}}
}


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
\alias{addHandlerColumnDoubleclicked}
\alias{addHandlerColumnDoubleclicked.default}
\alias{addHandlerColumnRightclicked}
\alias{addHandlerColumnRightclicked.default}
\alias{addHandlerColumnclicked}
\alias{addHandlerColumnclicked.default}
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

  addHandler.default(obj, signal, handler, action = NULL,
  ...)

  addHandlerChanged(obj, handler, action = NULL, ...)

  addHandlerChanged.default(obj, handler, action = NULL,
  ...)

  addHandlerClicked(obj, handler, action = NULL, ...)

  addHandlerClicked.default(obj, handler, action = NULL,
  ...)

  addHandlerDoubleclick(obj, handler, action = NULL, ...)

  addHandlerDoubleclick.default(obj, handler, action =
  NULL, ...)

  addHandlerRightclick(obj, handler, action = NULL, ...)

  addHandlerRightclick.default(obj, handler, action = NULL,
  ...)

  addHandlerColumnclicked(obj, handler, action = NULL, ...)

  addHandlerColumnclicked.default(obj, handler, action =
  NULL, ...)

  addHandlerColumnDoubleclicked(obj, handler, action =
  NULL, ...)

  addHandlerColumnDoubleclicked.default(obj, handler,
  action = NULL, ...)

  addHandlerColumnRightclicked(obj, handler, action = NULL,
  ...)

  addHandlerColumnRightclicked.default(obj, handler, action
  = NULL, ...)

  addHandlerSelect(obj, handler, action = NULL, ...)

  addHandlerSelect.default(obj, handler, action = NULL,
  ...)

  addHandlerFocus(obj, handler, action = NULL, ...)

  addHandlerFocus.default(obj, handler, action = NULL, ...)

  addHandlerBlur(obj, handler, action = NULL, ...)

  addHandlerBlur.default(obj, handler, action = NULL, ...)

  addHandlerDestroy(obj, handler, action = NULL, ...)

  addHandlerDestroy.default(obj, handler, action = NULL,
  ...)

  addHandlerUnrealize(obj, handler, action = NULL, ...)

  addHandlerUnrealize.default(obj, handler, action = NULL,
  ...)

  addHandlerExpose(obj, handler, action = NULL, ...)

  addHandlerExpose.default(obj, handler, action = NULL,
  ...)

  addHandlerKeystroke(obj, handler, action = NULL, ...)

  addHandlerKeystroke.default(obj, handler, action = NULL,
  ...)

  addHandlerMouseMotion(obj, handler, action = NULL, ...)

  addHandlerMouseMotion.default(obj, handler, action =
  NULL, ...)

  addHandlerIdle(...)

  addPopupMenu(obj, menulist, action = NULL, ...)

  addPopupMenu.default(obj, menulist, action = NULL, ...)

  add3rdmousePopupMenu(obj, menulist, action = NULL, ...)

  add3rdmousePopupMenu.default(obj, menulist, action =
  NULL, ...)

  addDropSource(obj, handler, action = NULL, ...)

  addDropSource.default(obj, handler, action = NULL, ...)

  addDropTarget(obj, handler, action = NULL, ...)

  addDropTarget.default(obj, handler, action = NULL, ...)

  addDragMotion(obj, handler, action = NULL, ...)

  addDragMotion.default(obj, handler, action = NULL, ...)

  blockHandlers(obj, ...)

  blockHandlers.default(obj, ...)

  blockHandler(obj, ID, ...)

  blockHandler.default(obj, ID, ...)

  unblockHandlers(obj, ...)

  unblockHandlers.default(obj, ...)

  unblockHandler(obj, ID, ...)

  unblockHandler.default(obj, ID, ...)

  removeHandler(obj, ID, ...)

  removeHandler.default(obj, ID, ...)
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

  \item{menulist}{a list of \code{gaction} items}

  \item{ID}{returned by addHandler. If missing will try to
  block handler passed to constructor}
}
\value{
  a handler ID which can be used to block/unblock or remove
  the handler

  NULL
}
\description{
  Generic method to add a handler passing a signal

  Default S3 method

  The "changed" event is also the one that a handler passed
  to the constructor is called on

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


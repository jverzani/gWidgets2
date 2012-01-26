\name{gvarbrowser}
\alias{.gvarbrowser}
\alias{gvarbrowser}
\alias{svalue.GVarBrowser}
\title{Constructor for workspace variable browser}
\usage{
  gvarbrowser(handler = NULL, action = "summary", container
  = NULL, ..., toolkit = guiToolkit())

  .gvarbrowser(toolkit, handler = NULL, action = "summary",
  container = NULL, ...)

  \method{svalue}{GVarBrowser} (obj, index = FALSE, drop =
  TRUE, ...)
}
\arguments{
  \item{handler}{A handler assigned to the default change
  signal. Handlers are called when some event triggers a
  widget to emit a signal. For each widget some default
  signal is assumed, and handlers may be assigned to that
  through \code{addHandlerChanged} or at construction time.
  Handlers are functions whose first argument, \code{h} in
  the documentation, is a list with atleast two components
  \code{obj}, referring to the object emitting the signal
  and \code{action}, which passes in user-specified data to
  parameterize the function call.}

  \item{action}{User supplied data passed to the handler
  when it is called}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations.}

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}

  \item{obj}{object of method call}

  \item{index}{NULL or logical. If \code{TRUE} and widget
  supports it an index, instead of a value will be
  returned.}

  \item{drop}{NULL or logical. If widget supports it, drop
  will work as it does in a data frame or perhaps someother
  means.}
}
\description{
  A workspace browser widget.  The workspace browser
  displays values in the global environment. The way these
  values are displayed may be adjusted through the option
  \code{gwidgets2:gvarbrowser_classes}. The default is
  \code{gWidgets2:::gvarbrowser_default_classes}. This
  value is a named list, where the component is a character
  vector of classes to display for that category. Non
  categorized values appear separately.

  generic for toolkit dispatch

  Return selected objects a string (when \code{drop=TRUE})
  with recursive values separated by \code{$}, or the
  objects themselves (when \code{drop=FALSE}).
}
\details{
  The variable browser uses an instance of
  \code{WSWatcherModel} to monitor the global workspace.
  This instance may be useful for other purposes. (For
  example, one may add an observer that is called to listen
  for changes to the set of available data frames.). The
  instance is available through the \code{ws_model}
  property.
}


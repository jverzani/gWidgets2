\name{gformlayout}
\alias{.gformlayout}
\alias{gformlayout}
\alias{svalue.GFormLayout}
\title{A form layout container}
\usage{
  gformlayout(align = c("default", "left", "center"),
    spacing = 5, container = NULL, ...,
    toolkit = guiToolkit())

  .gformlayout(toolkit, align = "left", spacing = 5,
    container = NULL, ...)

  \method{svalue}{GFormLayout}(obj, index = NULL,
    drop = NULL, ...)
}
\arguments{
  \item{align}{alignment of label. Left justify or center
  balance. Leave as "default" for underlying toolkit
  default.}

  \item{spacing}{spacing between columns}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations. For example, when using a widget as a
  menubar object one can specify a parent argument to pass
  in parent information, similar to how the argument is
  used with gaction and the dialogs.}

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
  This convenience container is basically a simpler form of
  \code{glayout} to be used to layout two columns forms
  with a label on the left.  The label can be passed in to
  the \code{add} method of the container as is done with
  notebook labels

  .gformlayout generic for toolkit dispatch

  The \code{svalue} method for \code{GFormLayout} returns a
  list of values created by calling \code{svalue} on each
  child. The returned list is named by the corresponding
  labels.
}


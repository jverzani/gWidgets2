\name{gframe}
\alias{.gframe}
\alias{gframe}
\title{Constructor for framed box container with label}
\usage{
  gframe(text = "", markup = FALSE, pos = 0,
    horizontal = TRUE, spacing = 5, container = NULL, ...,
    toolkit = guiToolkit())

  .gframe(toolkit, text = "", markup = FALSE, pos = 0,
    horizontal = TRUE, spacing = 5, container = NULL, ...)
}
\arguments{
  \item{text}{frame label}

  \item{markup}{does label use markup (toolkit specific)}

  \item{pos}{position of label: 0=left, 1=right, some
  toolkit allow values in between}

  \item{toolkit}{toolkit}

  \item{horizontal}{logical. If TRUE, left to right layout,
  otherwise top to bottom}

  \item{spacing}{spacing aroud widget}

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
}
\description{
  The framed box container inherits from \code{ggroup}. The
  main addition is a label, which is accessed via the
  \code{name} method.

  generic for toolkit dispatch
}
\note{
  to include a scrollwindow, place a \code{ggroup} within
  this window.
}
\seealso{
  \code{\link{ggroup}} and \code{\link{gexpandgroup}}
}


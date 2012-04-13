\name{gstackwidget}
\alias{.gstackwidget}
\alias{gstackwidget}
\title{Constructor for a stack of widgets}
\usage{
  gstackwidget(container = NULL, ..., toolkit = guiToolkit())

  .gstackwidget(toolkit, container = NULL, ...)
}
\arguments{
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
}
\description{
  This widget is like a notebook -- it holds a stack of pages, but does not provide the tabs to work with. Most methods are inherited from gnotebook's.

  generic for toolkit dispatch
}


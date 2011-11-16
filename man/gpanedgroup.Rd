\name{gpanedgroup}
\alias{.gpanedgroup}
\alias{gpanedgroup}
\title{constructor for a two-paned container}
\usage{
  gpanedgroup(horizontal = TRUE, container = NULL, ...,
    toolkit = guiToolkit())

  .gpanedgroup(toolkit, horizontal = TRUE,
    container = NULL, ...)
}
\arguments{
  \item{horizontal}{direction of layout}

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
  Holds two child widgets.

  generic for toolkit dispatch
}
\note{
  There were arguments to specify the children at
  construction, but these have been removed.
}


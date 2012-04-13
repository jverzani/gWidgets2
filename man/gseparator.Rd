\name{gseparator}
\alias{.gseparator}
\alias{gseparator}
\title{constructor providing a widget for displaying a line in a GUI}
\usage{
  gseparator(horizontal = TRUE, container = NULL, ..., toolkit = guiToolkit())

  .gseparator(toolkit, horizontal = TRUE, container = NULL, ...)
}
\arguments{
  \item{horizontal}{Logical. Is separator drawn
  horizontally?}

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
  constructor providing a widget for displaying a line in a GUI

  generic for toolkit dispatch
}


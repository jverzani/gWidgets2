\name{ghtml}
\alias{.ghtml}
\alias{ghtml}
\title{Widget for HTML display}
\usage{
  ghtml(x, container = NULL, ..., toolkit = guiToolkit())

  .ghtml(toolkit, x, container = NULL, ...)
}
\arguments{
  \item{x}{url or character vector of HTML formatted text.
  URLs marked by "http://" prefix}

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
}
\value{
  a \code{GHtml} instance.
}
\description{
  Widget for HTML display

  S3 generic whose methods are implemented in the toolkit
  packages
}
\author{
  john verzani
}


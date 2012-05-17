\name{gstatusbar}
\alias{.gstatusbar}
\alias{gstatusbar}
\title{constructor to add a status bar to main window}
\usage{
  gstatusbar(text = "", container = NULL, ...,
    toolkit = guiToolkit())

  .gstatusbar(toolkit, text = "", container = NULL, ...)
}
\arguments{
  \item{text}{inital status text}

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
  constructor to add a status bar to main window

  generic for toolkit dispatch
}
\examples{
\dontrun{
}
}


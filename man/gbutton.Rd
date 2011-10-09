\name{gbutton}
\alias{.gbutton}
\alias{gbutton}
\title{Basic button widget}
\usage{
  gbutton(text = "", handler = NULL, action = NULL,
  container = NULL, ..., toolkit = guiToolkit())

  .gbutton(toolkit, text, handler, action, container, ...)
}
\arguments{
  \item{text}{label text. If text matches a stock icon
  name, that is used as well}
}
\value{
  a \code{GButton} instance. While this object has its own
  (reference) methods, one primarily interacts with it
  through S3 methods defined within the package.
}
\description{
  The basic button widget is a common means to allow the
  user to invoke an action. This action may be specified by
  a handler or by a \code{GAction} object. The main
  property for \code{GButton} is the label text. If this
  text matches a stock icon name and the toolkit supports
  it, an icon will accompany the button.

  S3 generic whose methods are implemented in the toolkit
  packages
}


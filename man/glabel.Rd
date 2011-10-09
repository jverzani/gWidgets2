\name{glabel}
\alias{.glabel}
\alias{glabel}
\alias{svalue<-.GLabel}
\title{Basic label widget}
\usage{
  glabel(text = "", markup = FALSE, editable = FALSE,
  handler = NULL, action = NULL, container = NULL, ...,
  toolkit = guiToolkit())

  .glabel(toolkit, text, markup = FALSE, editable = FALSE,
  handler = NULL, action = NULL, container = NULL, ...)
}
\arguments{
  \item{text}{character. Collapsed using a newline to a
  single string.}

  \item{markup}{logical. If toolkit supports markup, this
  indicates it will be used}

  \item{editable.}{If TRUE, then clicking on label will
  enable editing of value}

  \item{handler}{optional handler. If given added through
  addHandlerChanged. Overridden if \code{editable=TRUE}.}

  \item{action}{passed to handler through \code{action}
  component of first argument of handler. For buttons, this
  may also be a \code{GAction} instance.}

  \item{container}{parent container (Optional for some
  toolkits, but not all).}

  \item{...}{passed to \code{add} method of parent
  container}

  \item{toolkit}{toolkit instance}

  \item{value}{integer amount of spacing}
}
\value{
  a \code{GLabel} instance. While this object has its own
  (reference) methods, one primarily interacts with it
  through S3 methods defined within the package.
}
\description{
  The basic label widget allows one to label areas of a GUI
  using text. The most common use would be to label fields
  in a form. For \pkg{gWidgets2} labels may be editable or
  responsive to mouse clicks, although it is the author's
  experience that such uses are not expected by the end
  user.

  S3 generic whose methods are implemented in the toolkit
  packages

  Set the main property of the label, its text
}
\author{
  john verzani
}


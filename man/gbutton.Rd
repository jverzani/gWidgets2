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

  \item{handler}{optional handler. If given added through
  addHandlerChanged}

  \item{action}{passed to handler through \code{action}
  component of first argument of handler. For buttons, this
  may also be a \code{GAction} instance.}

  \item{container}{parent container (Optional for some
  toolkits, but not all).}

  \item{...}{passed to \code{add} method of parent
  container}

  \item{toolkit}{toolkit instance}
}
\value{
  a \code{GButton} instance. While this object has its own
  (reference) methods, one primarily interacts with it
  through S3 methods defined within the package.
}
\description{
  button widget

  S3 generic whose methods are implemented in the toolkit
  packages
}
\note{
  removed the border option. This may be supported in the
  toolkit object
}


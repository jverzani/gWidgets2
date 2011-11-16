\name{gexpandgroup}
\alias{.gexpandgroup}
\alias{gexpandgroup}
\title{Constructor of box container widget with disclosure trigger and label}
\usage{
  gexpandgroup(text = "", markup = FALSE,
    horizontal = TRUE, handler = NULL, action = NULL,
    container = NULL, ..., toolkit = guiToolkit())

  .gexpandgroup(toolkit, text = "", markup = FALSE,
    horizontal = TRUE, handler = NULL, action = NULL,
    container = NULL, ...)
}
\arguments{
  \item{text}{Label text}

  \item{markup}{logical. Does text have markup. (Toolkit
  dependent)}

  \item{horizontal}{horizontal (\code{TRUE}) or vertical
  packing.}

  \item{handler}{handler called when toggled}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to parent's \code{add} method}

  \item{toolkit}{toolkit}
}
\value{
  An object of class \code{GExpandGroup} inheriting from
  \code{GFrame} its methods and overrides:

  \enumerate{

  \item \code{visible<-} Logical. To specify if widget is
  open (\code{TRUE}) or closed.

  }
}
\description{
  Constructor of box container widget with disclosure
  trigger and label

  generic for toolkit dispatch
}
\seealso{
  \code{\link{ggroup}} and \code{\link{gframe}}
}


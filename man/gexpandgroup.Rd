\name{gexpandgroup}
\alias{.gexpandgroup}
\alias{addHandlerChanged.GExpandGroup}
\alias{gexpandgroup}
\alias{visible<-.GExpandGroup}
\title{Constructor of box container widget with disclosure trigger and label}
\usage{
  gexpandgroup(text = "", markup = FALSE, horizontal =
  TRUE, handler = NULL, action = NULL, container = NULL,
  ..., toolkit = guiToolkit())

  .gexpandgroup(toolkit, text = "", markup = FALSE,
  horizontal = TRUE, handler = NULL, action = NULL,
  container = NULL, ...)

  \method{addHandlerChanged}{GExpandGroup} (obj, handler,
  action = NULL, ...)
}
\arguments{
  \item{text}{Label text}

  \item{markup}{logical. Does text have markup. (Toolkit
  dependent)}

  \item{horizontal}{horizontal (\code{TRUE}) or vertical
  packing.}

  \item{handler}{handler called when state is toggled}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to parent's \code{add} method}

  \item{toolkit}{toolkit}

  \item{obj}{object receiving event and emitting a signal
  to the handler}
}
\value{
  An object of class \code{GExpandGroup} inheriting from
  \code{GFrame}
}
\description{
  Constructor of box container widget with disclosure
  trigger and label

  generic for toolkit dispatch

  For gexapndgroup, the visible assignment method is used
  to change the disclosure state

  The change handler for a expandGroup is called when the
  group changes visibility
}
\seealso{
  \code{\link{ggroup}} and \code{\link{gframe}}
}


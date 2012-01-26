\name{gframe}
\alias{.gframe}
\alias{gframe}
\title{Constructor for framed box container with label}
\usage{
  gframe(text = "", markup = FALSE, pos = 0, horizontal =
  TRUE, container = NULL, ..., toolkit = guiToolkit())

  .gframe(toolkit, text = "", markup = FALSE, pos = 0,
  horizontal = TRUE, container = NULL, ...)
}
\arguments{
  \item{text}{frame label}

  \item{markup}{does label use markup (toolkit specific)}

  \item{pos}{position of label: 0=left, 1=right, some
  toolkit allow values in between}

  \item{horizontal}{logical. Direction of layout}

  \item{container}{parent container}

  \item{...}{passed to add method of parent container}

  \item{toolkit}{toolkit}
}
\description{
  The framed box container inherits from \code{ggroup}. The
  main addition is a label, which is accessed via the
  \code{name} method.

  generic for toolkit dispatch
}
\seealso{
  \code{\link{ggroup}} and \code{\link{gexpandgroup}}
}


\name{ggroup}
\alias{.ggroup}
\alias{ggroup}
\alias{svalue<-.GGroup}
\title{Basic box container}
\usage{
  ggroup(horizontal = TRUE, spacing = 5, use.scrollwindow =
  FALSE, container = NULL, ..., toolkit = guiToolkit())

  .ggroup(toolkit, horizontal = TRUE, spacing = 5,
  use.scrollwindow = FALSE, container = NULL, ...)
}
\arguments{
  \item{horizontal}{logical. If TRUE, left to right layout,
  otherwise top to bottom}

  \item{spacing}{spacing aroud widget}

  \item{use.scrollwindow}{logical. If TRUE, places box
  container in scrollwindow. In this case, a size should be
  set.}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent
  container}

  \item{toolkit}{toolkit for dispatch}

  \item{obj}{\code{ggroup} instance}

  \item{index}{ignored}

  \item{...}{ignored}

  \item{value}{integer amount of spacing}
}
\value{
  a GGroup instance.
}
\description{
  Basic box container

  S3 generic whose methods are implemented in the toolkit
  packages

  The main property of the box container is the spacing.
  There are generally two types of spacing: padding around
  border of the box and spacing between each child that is
  packed in.
}


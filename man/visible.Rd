\name{visible}
\alias{visible}
\alias{visible.default}
\alias{visible<-}
\alias{visible<-.default}
\title{Controls whether widget is visible or not}
\usage{
  visible(obj, ...)

  \method{visible}{default} (obj, ...)

  visible(obj) <- value

  visible(obj) <- value
}
\arguments{
  \item{obj}{object}

  \item{...}{ignored}

  \item{value}{logical. Set visible state.}
}
\description{
  For most -- but not all -- widgets, a widget is visible
  if it is shown. For others, parts of the widget may be
  controlled by visible. If the former state is desired,
  simply place widget into a box container.

  Basic S3 method

  Set visibility of an object

  Basic S3 method for visible
}


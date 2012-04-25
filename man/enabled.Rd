\name{enabled}
\alias{enabled}
\alias{enabled.default}
\alias{enabled<-}
\alias{enabled<-.default}
\title{enabled}
\usage{
  enabled(obj)

  \method{enabled}{default}(obj)

  enabled(obj) <- value

  \method{enabled}{default}(obj, value) <- value
}
\arguments{
  \item{obj}{object}

  \item{value}{logical}
}
\value{
  logical indicating if widget is enabled

  if \code{value} is logical and \code{FALSE} widget will
  be insensitive to user input and rendered in a muted
  state.
}
\description{
  A widget is enabled if it is sensitive to user input

  base S3 method for enabled.

  Set whether widget is enabled or not

  S3 method for setting enabled property
}


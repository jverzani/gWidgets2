\name{size}
\alias{size}
\alias{size.default}
\alias{size<-}
\alias{size<-.default}
\title{Return size (width and height) of widget}
\usage{
  size(obj)

  \method{size}{default} (obj)

  size(obj) <- value
}
\arguments{
  \item{obj}{object}

  \item{value}{size in pixels}
}
\description{
  Return size (width and height) of widget

  S3 method for size

  The size is specified in pixels (integers). Some toolkits
  allow -1 as a default, but not all.

  S3 method for size
}


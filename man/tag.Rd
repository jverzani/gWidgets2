\name{tag}
\alias{tag}
\alias{tag.default}
\alias{tag<-}
\alias{tag<-.default}
\title{get a persistent attribute for an object}
\usage{
  tag(obj, key)

  \method{tag}{default} (obj, key)

  tag(obj, key) <- value

  \method{tag}{default} (obj, key, value) <- value
}
\arguments{
  \item{obj}{object}

  \item{key}{character. Values are stored by key. If
  missing, all keys are returned.}

  \item{value}{to assign to key}
}
\description{
  get a persistent attribute for an object

  Basic S3 method

  Unlike \code{attr<-}, this method (essentially) stores the attribute in a reference to the object, not a copy. As such it can be used within function call (handlers) to assign values outside the scope of the function call.

  Basic S3 method
}


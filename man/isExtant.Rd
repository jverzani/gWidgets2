\name{isExtant}
\alias{isExtant}
\alias{isExtant.default}
\title{Check if widget is extant.}
\usage{
  isExtant(obj)

  \method{isExtant}{default}(obj)
}
\arguments{
  \item{obj}{object}
}
\description{
  Widgets can be destroyed, but their R object is still
  present. This is FALSE in that case.

  Basic S3 method for isExtant
}


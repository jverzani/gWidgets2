\name{gslider}
\alias{.gslider}
\alias{gslider}
\title{slider widget constructor}
\usage{
  gslider(from = 0, to = 100, by = 1, length.out = NULL,
  along.with = NULL, value = from[1], horizontal = TRUE,
  handler = NULL, action = NULL, container = NULL, ...,
  toolkit = guiToolkit())

  .gslider(toolkit, from = 0, to = 100, by = 1, value =
  from, horizontal = TRUE, handler = NULL, action = NULL,
  container = NULL, ...)
}
\arguments{
  \item{from}{If a number of length one then a starting
  point, in which case to, by are passed to \code{seq}.
  Otherwise a sequence of values for which
  sort(unique(from)) will order}

  \item{to}{ending point when from is starting point}

  \item{by}{step size if not specified by \code{from}}

  \item{length.out}{in place of by}

  \item{along.with}{in place of length.out}
}
\description{
  slider widget constructor

  generic for toolkit dispatch
}


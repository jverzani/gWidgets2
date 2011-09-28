\name{gpanedgroup}
\alias{.gpanedgroup}
\alias{gpanedgroup}
\title{constructor for a two-paned container}
\usage{
  gpanedgroup(horizontal = TRUE, container = NULL, ...,
  toolkit = guiToolkit())

  .gpanedgroup(toolkit, horizontal = TRUE, container =
  NULL, ...)
}
\arguments{
  \item{horizontal}{direction of layout}
}
\description{
  Holds two child widgets.

  generic for toolkit dispatch
}
\note{
  There were arguments to specify the children at
  construction, but these have been removed.
}


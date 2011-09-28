\name{glayout}
\alias{.glayout}
\alias{[.GLayout}
\alias{[<-.GLayout}
\alias{glayout}
\title{Constructor for grid layout container}
\usage{
  glayout(homogeneous = FALSE, spacing = 10, container =
  NULL, ..., toolkit = guiToolkit())

  .glayout(toolkit, homogeneous = FALSE, spacing = 10,
  container = NULL, ...)
}
\arguments{
  \item{x}{glayout object}

  \item{i}{row index for adding}

  \item{j}{column index for adding}

  \item{...}{used to pass in values for expand, fill,
  anchor (see the \code{add} method of \code{ggroup}) for
  their meaning).}

  \item{value}{a component to add.}
}
\description{
  Constructor for grid layout container

  generic for toolkit dispatch

  pass back item, list or matrix of items depending on
  dimension

  The matrix notation allows for spanning of multiple rows
  and or columns, but no holes.
}


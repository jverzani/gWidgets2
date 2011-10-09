\name{length.GComponent}
\alias{[.GComponent}
\alias{[.GContainer}
\alias{[<-.GComponent}
\alias{dim.GComponent}
\alias{dimnames.GComponent}
\alias{dimnames<-.GComponent}
\alias{length.GComponent}
\alias{length<-.GComponent}
\alias{names.GComponent}
\alias{names<-.GComponent}
\alias{update.GComponent}
\title{Length of object}
\usage{
  length.GComponent(x)

  dim.GComponent(x)

  names.GComponent(x)

  dimnames.GComponent(x)

  update.GComponent(object, ...)
}
\arguments{
  \item{x}{object}

  \item{x}{\code{GComponent} object}

  \item{i}{main index or row index (if implemented)}

  \item{j}{column index (if implemented)}

  \item{...}{ignored}

  \item{drop}{logical. If \code{TRUE} return value is
  simplified, if possible.}

  \item{value}{value to set}
}
\value{
  length of object
}
\description{
  Length of object

  set length of object

  dimension of object

  Names are used in many different contexts.

  set names of object

  Get dimnames of object

  Set dimnames of object

  Get items of object

  Return children of a parent container

  Set object's items

  The update method will cause a widget to recompute
  itself, if it is necessary.
}


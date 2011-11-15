\name{length<-.GComponent}
\alias{[.GComponent}
\alias{[.GContainer}
\alias{[<-.GComponent}
\alias{dim.GComponent}
\alias{dimnames.GComponent}
\alias{dimnames<-.GComponent}
\alias{length<-.GComponent}
\alias{names.GComponent}
\alias{names<-.GComponent}
\alias{str.GComponent}
\alias{update.GComponent}
\title{set length of object}
\usage{
  \method{dim}{GComponent} (x)

  \method{names}{GComponent} (x)

  \method{dimnames}{GComponent} (x)

  \method{update}{GComponent} (object, ...)

  \method{str}{GComponent} (object, ...)
}
\arguments{
  \item{x}{component}

  \item{...}{dots argument}

  \item{object}{object to update}
}
\description{
  set length of object

  dimension of object

  Names are used in many different contexts.

  set names of object

  Get dimnames of object

  Set dimnames of object

  We use the extraction operator, \code{[}, typically to
  refer to the underlying items from which a selection can
  be made. As well, we overload this to containers to refer
  to the child components.

  Return children of a parent container

  Set object's items

  The update method will ca use a widget to recompute
  itself, if it is necessary.

  str method for widgets
}


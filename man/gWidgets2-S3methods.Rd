\name{length.GComponent}
\alias{[.GComponent}
\alias{[.GContainer}
\alias{[.GDf}
\alias{[.GEdit}
\alias{[<-.GComponent}
\alias{dim.GComponent}
\alias{dimnames.GComponent}
\alias{dimnames<-.GComponent}
\alias{length.GComponent}
\alias{length<-.GComponent}
\alias{names.GComponent}
\alias{names<-.GComponent}
\alias{names<-.GFrame}
\alias{str.GComponent}
\alias{update.GComponent}
\title{Length of object}
\usage{
  \method{length}{GComponent} (x)

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

  We use the extraction operator, \code{[}, typically to
  refer to the underlying items from which a selection can
  be made. As well, we overload this to containers to refer
  to the child components.

  Return children of a parent container

  Set object's items

  The update method will ca use a widget to recompute
  itself, if it is necessary.

  str method for widgets

  The current items for a \code{gdf} object are both the
  visible and non-visible items. To retrieve just the
  currently visible items, use the idiom
  \code{obj[visible(obj), ]}.

  The underlying widget may allow autocompletion, if this
  is the case then this method is used to set the list of
  candidates.

  set names for frame
}


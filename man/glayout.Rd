\name{glayout}
\alias{.glayout}
\alias{[.GLayout}
\alias{[<-.GLayout}
\alias{glayout}
\title{Constructor for grid layout container}
\usage{
  glayout(homogeneous = FALSE, spacing = 10, container = NULL, ..., toolkit = guiToolkit())

  .glayout(toolkit, homogeneous = FALSE, spacing = 10, container = NULL, ...)

  \method{[}{GLayout} (x, i, j, ..., drop = TRUE)

  \method{[}{GLayout} (x, i, j, ..., value) <- value
}
\arguments{
  \item{homogeneous}{are cells all the same size}

  \item{spacing}{between cell spacing}

  \item{x}{object}

  \item{i}{row index}

  \item{j}{column index}

  \item{drop}{drop return type?}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations. For example, when using a widget as a
  menubar object one can specify a parent argument to pass
  in parent information, similar to how the argument is
  used with gaction and the dialogs.}

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}
}
\description{
  Constructor for grid layout container

  generic for toolkit dispatch

  pass back item, list or matrix of items depending on dimension

  The matrix notation allows for spanning of multiple rows and or columns, but no holes. The \code{...} argument is used to pass in values for expand, fill, anchor (see the \code{add} method of \code{ggroup}) for their meaning).
}


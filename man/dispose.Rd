\name{dispose}
\alias{dispose}
\alias{dispose.GComponent}
\title{Dispose of object}
\usage{
  dispose(obj, ...)

  \method{dispose}{GComponent}(obj, ...)
}
\arguments{
  \item{obj}{object to dispose}

  \item{...}{passed along}
}
\description{
  Dispose of object, primarily a window though this is
  modified in \code{GNoteBook} and \code{GText}.

  main dispose method. Calls dispose for GWindow
}


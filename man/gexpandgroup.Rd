\name{addHandlerChanged.GExpandGroup}
\alias{addHandlerChanged.GExpandGroup}
\title{The change handler for a expandGroup is called when the group changes visibility}
\usage{
  \method{addHandlerChanged}{GExpandGroup} (obj, handler,
  action = NULL, ...)
}
\arguments{
  \item{obj}{object receiving event and emitting a signal
  to the handler}

  \item{handler}{handler to assign when signal is emitted.
  A handler is a function, its first argument should expect
  a list with components \code{obj} containing a reference
  to the object and \code{action}. Some handlers are passed
  additional values.}

  \item{action}{passed to handler to parameterize call.}

  \item{...}{passed along}
}
\description{
  The change handler for a expandGroup is called when the
  group changes visibility
}


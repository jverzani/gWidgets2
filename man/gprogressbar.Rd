\name{gprogressbar}
\alias{.gprogressbar}
\alias{gprogressbar}
\alias{svalue.GProgressBar}
\title{Basic progress bar widget}
\usage{
  gprogressbar(value = 10, container = NULL, ...,
    toolkit = guiToolkit())

  .gprogressbar(toolkit, value, container, ...)

  \method{svalue}{GProgressBar}(obj, index = NULL,
    drop = NULL, ...)
}
\arguments{
  \item{value}{Initial value, between 0 and 100}

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

  \item{obj}{object of method call}

  \item{index}{NULL or logical. If \code{TRUE} and widget
  supports it an index, instead of a value will be
  returned.}

  \item{drop}{NULL or logical. If widget supports it, drop
  will work as it does in a data frame or perhaps someother
  means.}
}
\value{
  a \code{GButton} instance. While this object has its own
  (reference) methods, one primarily interacts with it
  through S3 methods defined within the package.
}
\description{
  Basic progress bar widget

  S3 generic whose methods are implemented in the toolkit
  packages

  The \code{svalue} method for a button object refers to
  its value between 0 and 100
}


\name{svalue}
\alias{svalue}
\alias{svalue.default}
\alias{svalue<-}
\alias{svalue<-.GToolBar}
\alias{svalue<-.default}
\title{svalue}
\usage{
  svalue(obj, index = FALSE, drop = NULL, ...)

  svalue.default(obj, index = NULL, drop = NULL, ...)
}
\arguments{
  \item{obj}{object of method call}

  \item{index}{NULL or logical. If \code{TRUE} and widget
  supports it an index, instead of a value will be
  returned.}

  \item{drop}{NULL or logical. If widget supports it, drop
  will work as it does in a data frame or perhaps someother
  means.}

  \item{...}{passed on to call}
}
\description{
  This returns the "selected" value in a widget. Selection
  varies from widget to widget, but should generally is
  what can be added to the widget by mouse click or typing.
  For some widgets, the extra argument ‘index=TRUE’
  will return the index of the selected value, not the
  value. For some widget, the argument ‘drop’ is given
  to either prevent or encourage dropping of information.

  Calls coerce_with when available

  This method sets the main property of the widget.

  Base S3 method

  for a toolbar, \code{svalue<-} replaces the toolbar items
  with new ones specified by value.

  for a menubar, \code{svalue<-} replaces the menubar items
  with new ones specified by value.
}


\name{svalue}
\alias{svalue}
\alias{svalue.default}
\alias{svalue<-}
\alias{svalue<-.default}
\alias{svalue<-.GToolBar}
\title{svalue}
\usage{
  svalue(obj, index = FALSE, drop = NULL, ...)

  \method{svalue}{default}(obj, index = NULL, drop = NULL,
    ...)

  svalue(obj, index = TRUE, drop = TRUE, ..., value) <-
    value

  \method{svalue}{default}(obj, index = NULL, ..., value)
    <- value

  \method{svalue}{GToolBar}(obj, index = NULL, ..., value)
    <- value
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

  \item{value}{value to assign for selection or property}
}
\value{
  THe return value varies, depending if the widget is a
  "selection" widget or not. For non-selection widgets, the
  main property is loosely defined (the title of a window,
  text of a label or button, spacing of box containers,
  ...). For selection widgets the return value is the
  currently selected value. If no selection is made, this
  will be a 0-length vector with the expected class, if
  possible. For selection widgets, when \code{index=TRUE},
  the value is an integer, possible 0-length when non
  selection is made.
}
\description{
  This returns the "selected" value in a widget (if
  applicable), or its main property. Selection varies from
  widget to widget, but should generally be what can be
  added to the widget by mouse click or typing. For some
  widgets, the extra argument \code{index=TRUE} will return
  the index of the selected value, not the value. For some
  widget, the argument \code{drop} is given to either
  prevent or encourage dropping of information.

  Calls \code{coerce_with} when available. This value is a
  function and may be set as any property if the
  constructor does not explicity provide it.

  This method sets the selected value of, or main property
  of the widget.

  Base S3 method

  for a toolbar, \code{svalue<-} replaces the toolbar items
  with new ones specified by value.
}


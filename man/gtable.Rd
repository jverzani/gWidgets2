\name{gtable}
\alias{.gtable}
\alias{addHandlerChanged.GTable}
\alias{gtable}
\alias{svalue.GTable}
\alias{visible.GTable}
\title{A constructor for displaying tabular data for selection}
\usage{
  gtable(items, multiple = FALSE, chosen.col = 1, icon.col
  = NULL, tooltip.col = NULL, handler = NULL, action =
  NULL, container = NULL, ..., toolkit = guiToolkit())

  .gtable(toolkit, items, multiple = FALSE, chosen.col = 1,
  icon.col = NULL, tooltip.col = NULL, handler = NULL,
  action = NULL, container = NULL, ...)

  svalue.GTable(obj, index = NULL, ..., value)

  addHandlerChanged.GTable(obj, handler, action = NULL,
  ...)

  visible.GTable(obj, ...)
}
\arguments{
  \item{items}{data.frame specifies items for selection.
  May be a vector, matrix or data frame}

  \item{multiple}{logical allow multiple selectino}

  \item{chosen.col}{which value from the row is returned by
  selection}

  \item{icon.col}{NULL or integer. If latter, specifies
  column containing stock icon}

  \item{tooltip.col}{NULL or integer. If latter, specifies
  column containing tooltip}

  \item{InheritParams}{gwidget}

  \item{...}{passed on to call}

  \item{obj}{object of method call}

  \item{index}{NULL or logical. If \code{TRUE} and widget
  supports it an index, instead of a value will be
  returned.}

  \item{handler}{handler to assign when signal is emitted.
  A handler is a function, its first argument should expect
  a list with components \code{obj} containing a reference
  to the object and \code{action}. Some handlers are passed
  additional values.}

  \item{action}{passed to handler to parameterize call.}
}
\description{
  The tabular widget allows a user to select one (or more)
  row(s) using the mouse or keyboard selection. The
  selected rows are the main property and are returned by
  svalue through their key (from the column specified by
  \code{chosen.col}), or by index. The change handler
  changes on double clicke event. Use add handler click to
  respond to a change in selection.

  .gtable generic for toolkit dispatch

  For gtable one can pass in row(s) to select by index
  (when index=TRUE) or by match among the values in the
  chosen column

  The change handler for \code{GTable} is called when a row
  is activated, such as when it is double clicked. A single
  click is for adjusting the selection.

  For \code{GTable}, visibility refers to which rows are
  currently shown, not whether the widget itself is shown
  or hidden. (For the latter, place the widget into a
  container and adjust that). One can use this method to
  perform filtering by adjusting which rows are displayed
  according to some criteria that returns a logical.
}


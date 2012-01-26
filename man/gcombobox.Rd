\name{gcombobox}
\alias{.gcombobox}
\alias{.make_gcombobox_items}
\alias{[<-.GComboBox}
\alias{addHandlerChanged.GComboBox}
\alias{gcombobox}
\alias{gdroplist}
\alias{svalue.GComboBox}
\title{constructor for combobox}
\usage{
  gcombobox(items, selected = 1, editable = FALSE,
  coerce.with = NULL, handler = NULL, action = NULL,
  container = NULL, ..., toolkit = guiToolkit())

  .gcombobox(toolkit, items, selected = 1, editable =
  FALSE, coerce.with = NULL, handler = NULL, action = NULL,
  container = NULL, ...)

  gdroplist(...)

  .make_gcombobox_items(value)

  \method{addHandlerChanged}{GComboBox} (obj, handler,
  action = NULL, ...)

  \method{svalue}{GComboBox} (obj, index = NULL, drop =
  NULL, ...)
}
\arguments{
  \item{items}{Items to select from. A vector or a data
  frame. If a data frame, then first column is values.
  Second is optional, but can specify a stock icon name,
  third is optional and can be used to specify a tooltip.
  These may not be supported in all toolkits.}

  \item{selected}{integer. Which item (by index) is
  selected. Use -1 for no selection}

  \item{editable}{logical. Is user allowed to edit value}

  \item{coerce.with}{A function of function name to be
  called before selected value is returned by
  \code{svalue}}

  \item{handler}{Called when combobox value is changed.}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to parent container's \code{add}
  method}

  \item{toolkit}{toolkit}

  \item{value}{new items for selection}

  \item{drop}{For matrices and arrays.  If \code{TRUE} the
  result is coerced to the lowest possible dimension (see
  the examples).  This only works for extracting elements,
  not for the replacement.  See \code{\link{drop}} for
  further details.  }

  \item{obj}{object receiving event and emitting a signal
  to the handler}

  \item{index}{NULL or logical. If \code{TRUE} and widget
  supports it an index, instead of a value will be
  returned.}
}
\value{
  Returns an object of class \code{GComboBox} for which the
  following methods are overriden: \enumerate{ \item
  \code{svalue} Return selected value by name or (if
  \code{index=TRUE} by index). The latter only if
  \code{editable=FALSE}.

  \item \code{svalue<-} Set the selected value by value or
  if \code{index=TRUE} by index.

  \item \code{[} return items to select from

  \item \code{[<-} Set items to select from. }
}
\description{
  A combobox can be either a drop down list
  (editable=FALSE), or a drop-down list and edit area (a
  combobox).

  generic for toolkit dispatch

  Alias for gcombobox. Deprecated

  Non exported helper function to coerce items into a data
  frame. First column contains the values, second stock
  icons, third tooltips

  Ensure that value is a data frame. One can pass a vector
  or a one-column data frame to inidicate the possible
  values for selection, a second column is used for an
  icons (if possible), a third for a tooltip (if possible).

  Change handler for a non-editabe combobox is called when
  a new value is selected. For editable comboboxes, the
  handler is also called when the text entry box is
  activated.

  The \code{svalue} method for a combobox object refers to
  its main property, the selected value. When
  \code{index=FALSE} (or \code{NULL}) the value is
  returned. If \code{index=TRUE} the index of the object
  within the set of items is used.
}


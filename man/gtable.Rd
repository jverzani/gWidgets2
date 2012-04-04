\name{gtable}
\alias{.gtable}
\alias{[.GTable}
\alias{addHandlerChanged.GTable}
\alias{gtable}
\alias{size<-.GTable}
\alias{svalue.GTable}
\alias{visible.GTable}
\title{A constructor for displaying tabular data for selection}
\usage{
  gtable(items, multiple = FALSE, chosen.col = 1,
    icon.col = NULL, tooltip.col = NULL, handler = NULL,
    action = NULL, container = NULL, ...,
    toolkit = guiToolkit())

  .gtable(toolkit, items, multiple = FALSE, chosen.col = 1,
    icon.col = NULL, tooltip.col = NULL, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{svalue}{GTable} (obj, index = NULL, ..., value)

  \method{[}{GTable} (x, i, j, ..., drop = TRUE)

  \method{addHandlerChanged}{GTable} (obj, handler,
    action = NULL, ...)

  \method{visible}{GTable} (obj, ...)

  \method{size}{GTable} (obj, value) <- value
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

  \item{handler}{A handler assigned to the default change
  signal. Handlers are called when some event triggers a
  widget to emit a signal. For each widget some default
  signal is assumed, and handlers may be assigned to that
  through \code{addHandlerChanged} or at construction time.
  Handlers are functions whose first argument, \code{h} in
  the documentation, is a list with atleast two components
  \code{obj}, referring to the object emitting the signal
  and \code{action}, which passes in user-specified data to
  parameterize the function call.}

  \item{action}{User supplied data passed to the handler
  when it is called}

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

  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}
}
\value{
  Returns an object of class \code{GTable}
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
  chosen column. For setting by index, a value of 0L or
  integer(0) will clear the current selection

  For \code{GTable} objects the \code{[} and \code{[<-}
  methods are (mostly) implemented. The \code{[} method
  allows one to access the object using the regular matrix
  notation (but there is no list notation, e.g. \code{$} or
  \code{[[}, defined). The \code{[<-} method is available,
  but for most toolkits is restricted: one can not add
  columns, add rows, remove columns, remove rows, or change
  the type of the column. For all of these actions, one can
  reassign the items being displayed through the idiom
  \code{obj[] <- new_items}. This allows the widget to redo
  the column renderers.

  The change handler for \code{GTable} is called when the
  selection changes. This is often the result of a click
  event (but need not bo), although we alias to
  \code{addHandlerClicked}. For double click events, see
  \code{addHandlerDoubleclick}.

  For \code{GTable}, visibility refers to which rows are
  currently shown, not whether the widget itself is shown
  or hidden. (For the latter, place the widget into a
  container and adjust that). One can use this method to
  perform filtering by adjusting which rows are displayed
  according to some criteria that returns a logical.

  For \code{GTable} the \code{size<-} method is overridden
  to allow one to specify the column widths. To do so, pass
  in the values for \code{width}, \code{height} or
  \code{column.widths} as named components of a list. The
  value of \code{column.widths} should be a numeric vector
  of pixel widths of length matching the number of columns.
}


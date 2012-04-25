\name{gtree}
\alias{.gtree}
\alias{[.GTree}
\alias{gtree}
\alias{svalue.GTree}
\alias{svalue<-.GTree}
\alias{update.GTree}
\title{constructor for widget to display heirarchical data}
\usage{
  gtree(x = NULL, INDICES = NULL, offspring = NULL,
    offspring.data = NULL, chosen.col = 1,
    offspring.col = 2, icon.col = NULL, tooltip.col = NULL,
    multiple = FALSE, handler = NULL, action = NULL,
    container = NULL, ..., toolkit = guiToolkit())

  .gtree(toolkit, offspring = NULL, offspring.data = NULL,
    chosen.col = 1, offspring.col = 2, icon.col = NULL,
    tooltip.col = NULL, multiple = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{svalue}{GTree}(obj, index = FALSE, drop = TRUE,
    ...)

  \method{svalue}{GTree}(obj, index = TRUE, ..., value) <-
    value

  \method{[}{GTree}(x, i, j, ..., drop = FALSE)

  \method{update}{GTree}(object, ...)
}
\arguments{
  \item{x}{Data frame. Optional, if given specify INDICES
  value to split data into heirarchical data structure}

  \item{INDICES}{Integers or column names, referring to
  columns of \code{x}. Used to form heirarchical structure.
  Order is important.}

  \item{offspring}{function. A function passed values
  \code{path} and \code{data}, the latter from
  \code{offspring.data}. The path is the current position
  of the parent item using the named keys from the chosen
  column.}

  \item{offspring.data}{Passed to second argument of
  \code{offspring} function. Used to parameterize a
  function call.}

  \item{chosen.col}{integer or one of column names of data
  frame returned by \code{offspring}. The chosen column
  gives the key and value of the path.}

  \item{offspring.col}{integer or column name. Points to
  column containing logical values indicating if a row has
  offspring.}

  \item{icon.col}{integer of one of the column names of the
  data frame. If provided (non-NULL), then this column
  should provide a stock icon name to be placed in the row
  for the given data.}

  \item{tooltip.col}{integer or one of the column names of
  the data frame. If provided (non-NULL), then the row for
  this item will have a tooltip given by the pointed to
  value.}

  \item{multiple}{logical. Is multiple selection allowed?}

  \item{obj}{object}

  \item{index}{index}

  \item{drop}{drop}

  \item{i}{ignored}

  \item{j}{ignored}

  \item{drop}{do we return tip or path}

  \item{object}{object to update}

  \item{...}{passed to update method}

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

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}
}
\description{
  The \code{gtree} widget is used to present structured
  heirarchical data. This data may be specified through a
  data frame with some accompanying columns by which to
  split the data, or dynamically through a function
  (\code{offspring}).

  generic for toolkit dispatch

  For a \code{GTree} object, svalue refers to the path
  specified as a vector of keys or (if \code{INDEX=TRUE})
  by an integer vector of offspring positions. The
  \code{drop} argument is used to indicate if the terminus
  of the path is returned or the entire path,
  defaults=TRUE. To get the data associated with a row, use
  the \code{[} method.

  For a \code{GTree} object, svalue refers to the path
  specified as a vector of keys . For the assignment
  method, one assigns by index. That is \code{svalue(tr,
  index=TRUE) <- svalue(tr, index=TRUE)} should not change
  the state of the widget. (The \code{index=TRUE} argument
  is the case for setting, but not getting.)

  The \code{[} method is used to return the data associated
  with a selected row. The \code{svalue} method returns the
  path or its endpoint, the \code{[} method returns the row
  data associated with the path.

  The update method for \code{GTree} recomputes the base
  nodes, then reopens the given node if still available
}
\details{
  In the former case, the data frame is split up by the
  columns specified by INDICES. The first index is used to
  give the intial branches, the second index the second,
  etc. The end leaves are the data associated with a given
  path, with key given by that column specified by
  \code{chosen.col}

  For the latter case, the "path" of the current node (the
  node and its ancestors) is passed to the \code{offspring}
  function which computes the next level in the heirarchy.
  This level is specified through a data frame. This data
  frame has special columns. The \code{chosen.col}
  specifies which column is used as the key in the path,
  the \code{icon.col}(when given) points to a stock icon
  name to decorate the column. Similarly, the
  \code{tooltip.columns}. The fact that a row in the data
  frame has offspring is specified through the
  \code{offspring.col} column, again specifed by index or
  column name.
}


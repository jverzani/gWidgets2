\name{gnotebook}
\alias{.gnotebook}
\alias{add.GNotebook}
\alias{addHandlerChanged.GNotebook}
\alias{dispose.GNotebook}
\alias{gnotebook}
\alias{names.GNotebook}
\title{Constructor for a tabbed notebook container}
\usage{
  gnotebook(tab.pos = 3, container = NULL, ..., toolkit =
  guiToolkit())

  .gnotebook(toolkit, tab.pos = 3, container = NULL, ...)

  \method{add}{GNotebook} (obj, child, expand, fill,
  anchor, ...)

  \method{dispose}{GNotebook} (obj, ...)

  \method{addHandlerChanged}{GNotebook} (obj, handler,
  action = NULL, ...)
}
\arguments{
  \item{tab.pos}{integer. Position of tabs, 1 on bottom, 2
  left, 3 top, 4 right. (If supported)}

  \item{obj}{gnotebook object}

  \item{child}{some child component to add}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations.}

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}

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

  \item{expand}{NULL or logical. XXX}

  \item{fill}{NULL or logical. XXX}

  \item{anchor}{NULL or logical. XXX}
}
\value{
  none. called for its side effect.
}
\description{
  The tabbed notebook container allows one to hold many
  different pages with a mechanism to switch between them.
  In \code{gWidgets2} new pages are added through the
  \code{add} method. One passes in the tab label through
  the extra \code{label} argument, or subsequently through
  \code{names<-}.

  generic for toolkit dispatch

  Children added to notebooks need a label, a position and
  optionally a close button (if supported). The arguments
  expand, fill, anchor are not specified -- children expand
  and fill.

  Dispose deletes the current page, not the entire notebook
  object. To delete a specific page, a combination of
  \code{svalue<-} and \code{dispose} may be used.

  The \code{names} of a notebook are the page tab labels.
  These may be retrieved and set through the \code{names}
  method.

  the change handler for the notebook is called when the
  page changes. If possible, the new page number is passed
  back in the \code{page.no} component of 'h'
}
\note{
  In \pkg{gWidgets2} the button arguments of the
  \code{gWidgets} constructor are removed. One passes the
  close button request to the \code{add} method.

  To keep the signature the same as the generic, several
  arguments are passed in via ...:

  \describe{

  \item{label}{ A character. Label text for tab}

  \item{i}{An integer in \code{0} to \code{length(obj)}
  indicating the position to insert child. The new page is
  inserted to the right of page number \code{i}. When
  \code{i=0}, the page appears at the front, when \code{i}
  is not specified it appears at the end. }

  \item{close.button}{A logical. If \code{TRUE} -- and the
  toolkit supports it -- the page tab will include a close
  button. } }
}


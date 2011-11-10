\name{gnotebook}
\alias{.gnotebook}
\alias{add.GNotebook}
\alias{addHandlerChanged}
\alias{dispose.GNotebook}
\alias{gnotebook}
\alias{names.GNotebook}
\title{Constructor for a tabbed notebook container}
\usage{
  gnotebook(tab.pos = 3, container = NULL, ..., toolkit =
  guiToolkit())

  .gnotebook(toolkit, tab.pos = 3, container = NULL, ...)

  add.GNotebook(obj, child, expand, fill, anchor, ...)

  dispose.GNotebook(obj, ...)

  addHandlerChanged(obj, handler, action = NULL, ...)
}
\arguments{
  \item{tab.pos}{integer. Position of tabs, 1 on bottom, 2
  left, 3 top, 4 right. (If supported)}

  \item{obj}{gnotebook object}

  \item{child}{some child component to add}

  \item{...}{}
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
  arguments are passed in via ...: \describe{ \item{label}{
  A character. Label text for tab} \item{i}{An integer in
  \code{0} to \code{length(obj)} indicating the position to
  insert child. The new page is inserted to the right of
  page number \code{i}. When \code{i=0}, the page appears
  at the front, when \code{i} is not specified it appears
  at the end. \item{close.button}{A logical. If \code{TRUE}
  -- and the toolkit supports it -- the page tab will
  include a close button. }
}


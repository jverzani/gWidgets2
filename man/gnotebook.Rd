\name{gnotebook}
\alias{.gnotebook}
\alias{add.GNotebook}
\alias{addHandlerChanged}
\alias{dispose.GNotebook}
\alias{gnotebook}
\alias{names.GNotebook}
\alias{names<-.GNotebook}
\title{Constructor for a tabbed notebook container}
\usage{
  gnotebook(tab.pos = 3, container = NULL, ..., toolkit =
  guiToolkit())

  .gnotebook(toolkit, tab.pos = 3, container = NULL, ...)

  add.GNotebook(obj, child, label = "", i = length(obj),
  close.button = FALSE, ...)

  dispose.GNotebook(obj, ...)

  addHandlerChanged(obj, handler, action = NULL, ...)
}
\arguments{
  \item{tab.pos}{integer. Position of tabs, 1 on bottom, 2
  left, 3 top, 4 right. (If supported)}

  \item{obj}{gnotebook object}

  \item{child}{some child component to add}

  \item{label}{character. Label text for tab}

  \item{i}{integer. Position in 0..length(obj) to insert
  child. If empty at end, if 0 or less at beginning.}

  \item{close.button}{logical Do we add a close button
  (toolkit dependent)}

  \item{...}{}

  \item{x}{notebook object}

  \item{value}{new label text}
}
\value{
  adds child
}
\description{
  Constructor for a tabbed notebook container

  generic for toolkit dispatch

  Children added to notebooks need a label, a position and
  optionally a close button (if supported). The arguments
  expand, fill, anchor are not specified -- children expand
  and fill.

  Remove current page from notebook

  get tab names of notebook

  set tab names for object

  the change handler for the notebook is called when the
  page changes. If possible, the new page number is passed
  back in the \code{page.no} component of 'h'
}
\note{
  the button arguments of the gWidgets constructor are
  removed. One passes the close button request to the
  \code{add} method.
}


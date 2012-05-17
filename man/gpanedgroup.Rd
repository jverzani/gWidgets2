\name{gpanedgroup}
\alias{.gpanedgroup}
\alias{gpanedgroup}
\title{constructor for a two-paned container}
\usage{
  gpanedgroup(horizontal = TRUE, container = NULL, ...,
    toolkit = guiToolkit())

  .gpanedgroup(toolkit, horizontal = TRUE,
    container = NULL, ...)
}
\arguments{
  \item{horizontal}{direction of layout}

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
}
\description{
  A container for holding two child widgets where the space
  allocated to each can be manipulated by the user with a
  pane between the widgets, or programatticaly via
  \code{svalue<-}. Child widgets are added in the usual
  way, typically through the \code{container} argument of a
  constructor. Only two children may be added. Children
  expand and fill the allocated space.

  generic for toolkit dispatch
}
\note{
  There were arguments to specify the children at
  construction, but these have been removed.
}
\examples{
\dontrun{
w <- gwindow("gpanedgroup", visible=FALSE)
pg <- gpanedgroup(cont=w)
gbutton("left", cont=pg)
gbutton("right", cont=pg)

visible(w) <- TRUE
svalue(pg) <- 0.33
}
}


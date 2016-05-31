% Please edit documentation in R/gseparator.R
\name{gseparator}
\alias{.gseparator}
\alias{gseparator}
\title{constructor providing a widget for displaying a line in a GUI}
\usage{
gseparator(horizontal = TRUE, container = NULL, ...,
  toolkit = guiToolkit())

.gseparator(toolkit, horizontal = TRUE, container = NULL, ...)
}
\arguments{
\item{horizontal}{Logical. Is separator drawn horizontally?}

\item{container}{A parent container. When a widget is created it can be
incorporated into the widget heirarchy by passing in a parent
container at construction time. (For some toolkits this is not
optional, e.g. \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

\item{...}{These values are passed to the \code{add} method of the
parent container, and occasionally have been used to sneak in
hidden arguments to toolkit implementations.}

\item{toolkit}{Each widget constructor is passed in the toolkit it
will use. This is typically done using the default, which will
lookup the toolkit through \code{\link{guiToolkit}}.}
}
\description{
The \code{gseparator} widget provides a horizontal or vertical
line to visually divide child components of its parent
container. In addition to box containers this can be used within
toolbars (where one uses \code{parent} and not \code{container}).

generic for toolkit dispatch
}
\examples{
\dontrun{

w <- gwindow("Within page", visible=FALSE)
g <- gvbox(container=w)
glabel("Lorem ipsum ...", cont=g)
gseparator(cont=g)
bg <- ggroup(cont=g); addSpring(bg)
gbutton("dismiss", container=bg, handler=function(h,...) dispose(w))
visible(w) <- TRUE

w1 <- gwindow("within layout", visible=FALSE)
lyt <- glayout(container=w1)
lyt[1,1] <- "label"
lyt[2,1:2] <- gseparator(container=lyt)
lyt[3,2] <- "asdf"
visible(w1) <- TRUE

w2 <- gwindow("Within toolbar", visible=FALSE)
l <- list(file=gaction("File", parent=w2),
          sep=gseparator(parent=w2),
          quit=gaction("quit", parent=w2))
gtoolbar(l, cont=w2)
glabel("Lorem ipsum ...", container=w2)
visible(w2) <- TRUE
}
}


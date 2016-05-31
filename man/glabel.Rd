% Please edit documentation in R/glabel.R
\name{glabel}
\alias{.glabel}
\alias{glabel}
\alias{svalue<-.GLabel}
\title{Basic label widget}
\usage{
glabel(text = "", markup = FALSE, editable = FALSE, handler = NULL,
  action = NULL, container = NULL, ..., toolkit = guiToolkit())

.glabel(toolkit, text, markup = FALSE, editable = FALSE, handler = NULL,
  action = NULL, container = NULL, ...)

\method{svalue}{GLabel} (obj, index=TRUE, ...) <- value
}
\arguments{
\item{text}{character. Collapsed using a newline to a single string.}

\item{markup}{logical. If toolkit supports markup, this indicates
it will be used. It is suggested that the \code{font<-} method be
used, though for \pkg{gWidgets2Qt} \code{markup} is more
convenient.}

\item{editable}{If TRUE, then clicking on label will enable user-editing of the text.}

\item{handler}{optional handler. If given, added through addHandlerChanged. Overridden if \code{editable=TRUE}.}

\item{action}{passed to handler through \code{action} component of first argument of handler. For buttons, this may also be a \code{GAction} instance.}

\item{container}{parent container (Optional for some toolkits, but not all).}

\item{...}{passed to \code{add} method of parent container}

\item{toolkit}{toolkit instance}

\item{obj}{object of method call}

\item{index}{NULL or logical. If \code{TRUE} and widget supports it an index, instead of a value will be returned.}

\item{value}{value to assign for selection or property}
}
\value{
a \code{GLabel} instance. While this object has its own (reference) methods, one primarily interacts with it through S3 methods defined within the package.
}
\description{
The basic label widget allows one to label areas of a GUI using
text. The most common use would be to label fields in a form. For
\pkg{gWidgets2} labels may be editable or responsive to mouse
clicks, although it is the author's experience that such uses are
not expected by the end user.

S3 generic whose methods are implemented in the toolkit packages

The \code{svalue} methods refer to the main property of the label, its text.
}
\examples{
\dontrun{
w <- gwindow("gformlayout", visible=FALSE)
g <- gvbox(container=w)
g$set_borderwidth(10)

l1 <- glabel("static label", container=g)
l2 <- glabel("bold label", container=g)
font(l2) <- list(weight="bold")
l3 <- glabel("editable label. Click me", editable=TRUE, container=g)

visible(w) <- TRUE

}
}
\author{
john verzani
}


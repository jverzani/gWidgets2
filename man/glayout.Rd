% Please edit documentation in R/glayout.R
\name{glayout}
\alias{.glayout}
\alias{[.GLayout}
\alias{[<-.GLayout}
\alias{glayout}
\title{Constructor for grid layout container}
\usage{
glayout(homogeneous = FALSE, spacing = 10, container = NULL, ...,
  toolkit = guiToolkit())

.glayout(toolkit, homogeneous = FALSE, spacing = 10, container = NULL,
  ...)

\method{[}{GLayout}(x, i, j, ..., drop = TRUE)

\method{[}{GLayout} (x, i ,j, ...) <- value
}
\arguments{
\item{homogeneous}{are cells all the same size}

\item{spacing}{between cell spacing}

\item{container}{A parent container. When a widget is created it can be
incorporated into the widget heirarchy by passing in a parent
container at construction time. (For some toolkits this is not
optional, e.g. \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

\item{...}{These values are passed to the \code{add} method of the
parent container. Examples of values are \code{expand},
\code{fill}, and \code{anchor}, although they're not always
supported by a given widget. For more details see \link{add}.
Occasionally the variable arguments feature has been used to sneak
in hidden arguments to toolkit implementations. For example, when
using a widget as a menubar object one can specify a parent
argument to pass in parent information, similar to how the
argument is used with gaction and the dialogs.}

\item{toolkit}{Each widget constructor is passed in the toolkit it
will use. This is typically done using the default, which will
lookup the toolkit through \code{\link{guiToolkit}}.}

\item{x}{object}

\item{i}{row index}

\item{j}{column index}

\item{drop}{drop return type?}

\item{value}{constructor for a widget using this object as the
parent container}
}
\description{
The grid layout container uses matrix notation to position its
child components. This allows one to align widgets both
horizontally and vertically, as desired. There is some support for
matrix methods, such as \code{dim} and \code{[} to reference the
child objects.

generic for toolkit dispatch

The \code{[} method for the grid layout allows one to reference
the child objects by index. The return value is non standard. It
may be the item, a list (if one dimensonaL) or an array. The list
format is convenient to refer to all the child objects in a
column.

The matrix notation allows for spanning of multiple rows and or columns, but no holes.
The \code{...} argument is used to pass in values for expand, fill, anchor (see
the \code{add} method of \code{ggroup}) for their meaning).
}
\examples{
\dontrun{

w <- gwindow("glayout example", visible=FALSE)
g <- gvbox(container=w)
lyt <- glayout(container=g)
lyt[1,1] <- "a label"
lyt[1,2] <- gedit("A widget", container=lyt)
lyt[2, 1:2] <- gcombobox(state.name, cont=lyt)
g1 <- ggroup(container=g)
addSpring(g1)
gbutton("values", container=g1, handler=function(h, ...) {
  print(sapply(lyt[,2], svalue))
})
visible(w) <- TRUE

}
}
\seealso{
\code{\link{gformlayout}} for a more convenient means to layout forms.
}


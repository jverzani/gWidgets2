% Please edit documentation in R/gdf.R
\name{gdf}
\alias{.gdf}
\alias{addHandlerChanged.GDf}
\alias{gdf}
\alias{svalue.GDf}
\alias{visible<-.GDf}
\title{Constructor for a data frame editor}
\usage{
gdf(items = NULL, handler = NULL, action = NULL, container = NULL, ...,
  toolkit = guiToolkit())

.gdf(toolkit, items = NULL, handler = NULL, action = NULL,
  container = NULL, ...)

\method{addHandlerChanged}{GDf}(obj, handler, action = NULL, ...)

\method{svalue}{GDf}(obj, index = NULL, drop = TRUE, ...)

\method{visible}{GDf} (obj) <- value
}
\arguments{
\item{items}{data frame to edit}

\item{handler}{called on cell change}

\item{action}{passed to handler}

\item{container}{parent container}

\item{...}{passed to container's \code{add} method}

\item{toolkit}{toolkit}

\item{obj}{object receiving event and emitting a signal to the handler}

\item{index}{NULL or logical. If \code{TRUE} and widget supports it an index, instead of a value will be returned.}

\item{drop}{NULL or logical. If widget supports it, drop will work as it does in a data frame or perhaps someother means.}

\item{value}{value to assign for selection or property}
}
\value{
An object of class \code{gDf}.
}
\description{
Implementation varies wildly, but should provide at minimum
functionality of \code{edit.data.frame}. A single mouse click on a
cell should select that cell, a double click should initiate
editing of that cell.

generic for toolkit dispatch

Assign handler to be called when a cell, row or column changes

For gdf svalue refers to the selected values.

\code{visible} is used to refer to which rows are being shown.
}
\details{
Contents of the data frame can be accessed via \code{[} and manipulated with \code{[<-}.

The \code{save_data} reference class method can be called to
save the data into a variable with the specified name.
<<<<<<< HEAD
}
\examples{
library(gWidgets2)
if(interactive()) {
## An example of using some controls to modify which rows are displayed using visible<-

X <- mtcars

## Layout
w <- gwindow()
g <- gvbox(cont=w)

d <- gdf(X, cont=g)
b <- ggroup(cont=g)

nms <- names(X)
combo <- gcombobox(nms, cont=b)
glabel("==", cont=b)                    # could easily generalize
val <- gedit("", cont=b)

## Handlers.
## if entry widget is non-empty, find matches to display within selected variable
addHandlerChanged(val, handler=function(h,...) {
    if (svalue(combo, index=TRUE) < 1)
        return()
    value <- svalue(val)    
    if(nchar(value) == 0)
        return()

    var <- X[[svalue(combo)]]
    
    ind <- var == value
    visible(d) <- ind
})

## if a new variable is changed, update the dropdown list and
## reset value
addHandlerChanged(combo, handler=function(h,...) {
    vars <- sort(unique(X[[svalue(combo)]]))
    val[] <- vars
    svalue(val) == ""
})

}
=======

example in \code{inst/examples/ex-gdf.R}
>>>>>>> dadc65c1456c5a965be09dc4e6f8759bbf6ae9d3
}


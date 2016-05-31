% Please edit documentation in R/ghtml.R
\name{ghtml}
\alias{.ghtml}
\alias{ghtml}
\alias{svalue<-.GHtml}
\title{Widget for HTML display}
\usage{
ghtml(x, container = NULL, ..., toolkit = guiToolkit())

.ghtml(toolkit, x, container = NULL, ...)

\method{svalue}{GHtml} (obj, index=TRUE, ...) <- value
}
\arguments{
\item{x}{url or character vector of HTML formatted text. URLs marked by "http://" prefix}

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

\item{obj}{object of method call}

\item{index}{NULL or logical. If \code{TRUE} and widget supports it an index, instead of a value will be returned.}

\item{value}{value to assign for selection or property}
}
\value{
a \code{GHtml} instance.
}
\description{
This widget, when supported by the toolkit (not
\pkg{gWidgets2RGtk2} and \pkg{gWidgets2tcltk}) provides a simple
means to display HTML formatted text.

S3 generic whose methods are implemented in the toolkit packages

Use to update displayed content. Value is HTML fragment or url
}
\author{
john verzani
}


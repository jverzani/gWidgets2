% Please edit documentation in R/ggraphics.R
\name{ggraphics}
\alias{.ggraphics}
\alias{ggraphics}
\title{Constructor for an embeddable graphics device}
\usage{
ggraphics(width = dpi * 6, height = dpi * 6, dpi = 75, ps = 12,
  handler = NULL, action = NULL, container = NULL, ...,
  toolkit = guiToolkit())

.ggraphics(toolkit, width = dpi * 6, height = dpi * 6, dpi = 75,
  ps = 12, handler = NULL, action = NULL, container = NULL, ...)
}
\arguments{
\item{width}{width of device (pixels)}

\item{height}{hieght of widget (pixels)}

\item{dpi}{dots per inch}

\item{ps}{pointsize}

\item{handler}{A handler assigned to the default change
signal. Handlers are called when some event triggers a widget to
emit a signal. For each widget some default signal is assumed, and
handlers may be assigned to that through \code{addHandlerChanged}
or at construction time. Handlers are functions whose first
argument, \code{h} in the documentation, is a list with atleast
two components \code{obj}, referring to the object emitting the
signal and \code{action}, which passes in user-specified data to
parameterize the function call.

Handlers may also be added via \code{addHandlerXXX} methods for
the widgets, where \code{XXX} indicates the signal, with a default
signal mapped to \code{addHandlerChanged}
(cf. \code{\link{addHandler}} for a listing). These methods pass
back a handler ID that can be used with \code{blockHandler} and
\code{unblockHandler} to suppress temporarily the calling of the
handler.}

\item{action}{User supplied data passed to the handler when it is called}

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
}
\description{
Some toolkits provide an embeddable graphics device. When this is the case, this widget provides same.

generic for toolkit dispatch
}
\examples{
\dontrun{
## This shows how to use the device within a notebook

w <- gwindow("notebook example")
nb <- gnotebook(cont=w)

devs <- lapply(1:5, function(i) ggraphics(cont=nb, label=as.character(i)))

addHandlerChanged(nb, handler=function(h,...) {
  ## Tricky part is svalue(h$obj) is not the new page number -- but old
  ## so we use the pageno component here
    gg <- h$obj[h$pageno]
    visible(gg) <- TRUE
})


}
}


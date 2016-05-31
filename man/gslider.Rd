% Please edit documentation in R/gslider.R
\name{gslider}
\alias{.gslider}
\alias{gslider}
\title{slider widget constructor}
\usage{
gslider(from = 0, to = 100, by = 1, length.out = NULL,
  along.with = NULL, value = from[1], horizontal = TRUE, handler = NULL,
  action = NULL, container = NULL, ..., toolkit = guiToolkit())

.gslider(toolkit, from = 0, to = 100, by = 1, value = from,
  horizontal = TRUE, handler = NULL, action = NULL, container = NULL,
  ...)
}
\arguments{
\item{from}{If a number of length one then a starting point, in
which case to, by are passed to \code{seq}. Otherwise a sequence
of values for which sort(unique(from)) will order}

\item{to}{ending point when from is starting point}

\item{by}{step size if not specified by \code{from}}

\item{length.out}{in place of by}

\item{along.with}{in place of length.out}

\item{value}{initial value}

\item{horizontal}{Logical. Is separator drawn horizontally?}

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
A slider widgets allows a selection from a range of numeric
values. The widget presents the user with a quick to adjust, but
relatively difficult to adjust precisely way to to pick a number.

generic for toolkit dispatch
}
\examples{
if(interactive()) {
  ## a range widget uses either a slider or a linked spinbutton to select a value
  w <- gwindow("Range widget", visible=FALSE)
  g <- ggroup(cont=w, horizontal=TRUE)
  sl <- gslider(from=0, to=100, by=1, value=0, cont=g, expand=TRUE, fill="both")
  sp <- gspinbutton(from=0, to=100, by=1, value=0, cont=g)

  ## Two ways to do this:
  ##  addHandlerChanged(sl, function(...) svalue(sp) <- svalue(sl))
  ##  addHandlerChanged(sp, function(...) svalue(sl) <- svalue(sp))

  f <- function(h, ...) svalue(h$action) <- svalue(h$obj)
  addHandlerChanged(sl, f, action=sp)
  addHandlerChanged(sp, f, action=sl)
  
  visible(w) <- TRUE
}
}
\seealso{
\code{\link{gspinbutton}}
}


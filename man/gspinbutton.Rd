\name{gspinbutton}
\alias{.gspinbutton}
\alias{gspinbutton}
\title{Spinbutton constructor}
\usage{
  gspinbutton(from = 0, to = 10, by = 1, length.out = NULL, along.with = NULL, value = from, digits = 0, handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())

  .gspinbutton(toolkit, from = 0, to = 10, by = 1, value = from, digits = 0, handler = NULL, action = NULL, container = NULL, ...)
}
\arguments{
  \item{from}{from value}

  \item{to}{to value}

  \item{by}{step length}

  \item{length.out}{number of steps. Only one of \code{by}
  or \code{length.out} is used.}

  \item{along.with}{Take from}

  \item{value}{initial value}

  \item{digits}{number of digits to display, should the
  toolkit support it}

  \item{handler}{A handler assigned to the default change
  signal. Handlers are called when some event triggers a
  widget to emit a signal. For each widget some default
  signal is assumed, and handlers may be assigned to that
  through \code{addHandlerChanged} or at construction time.
  Handlers are functions whose first argument, \code{h} in
  the documentation, is a list with atleast two components
  \code{obj}, referring to the object emitting the signal
  and \code{action}, which passes in user-specified data to
  parameterize the function call.}

  \item{action}{User supplied data passed to the handler
  when it is called}

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
  Spinbutton constructor

  generic for toolkit dispatch
}
\examples{
if(interactive()) {
  ## a range widget uses either a slider or a linked spinbutton to select a value
  w <- gwindow("Range widget", visible=FALSE)
  g <- ggroup(cont=w, horizontal=TRUE)
  sl <- gslider(from=0, to=100, by=1, value=0, cont=g, expand=TRUE)
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
  \code{\link{gslider}}
}


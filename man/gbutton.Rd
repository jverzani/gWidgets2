\name{gbutton}
\alias{.gbutton}
\alias{gbutton}
\title{Basic button widget}
\usage{
  gbutton(text = "", handler = NULL, action = NULL,
  container = NULL, ..., toolkit = guiToolkit())

  .gbutton(toolkit, text, handler, action, container, ...)
}
\arguments{
  \item{text}{label text. If text matches a stock icon
  name, that is used as well}

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
  \pkg{gWidgets2tcltk} or \pkg{gWidgetsWWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations.}

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}
}
\value{
  a \code{GButton} instance. While this object has its own
  (reference) methods, one primarily interacts with it
  through S3 methods defined within the package.
}
\description{
  The basic button widget is a common means to allow the
  user to invoke an action. This action may be specified by
  a handler or by a \code{GAction} object. The main
  property for \code{GButton} is the label text. If this
  text matches a stock icon name and the toolkit supports
  it, an icon will accompany the button.

  S3 generic whose methods are implemented in the toolkit
  packages
}


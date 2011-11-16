\name{ggraphics}
\alias{.ggraphics}
\alias{ggraphics}
\title{Constructor for an embeddable graphics device}
\usage{
  ggraphics(width = dpi<c2><a0>*<c2><a0>6,
    height = dpi<c2><a0>*<c2><a0>6, dpi = 75, ps = 12,
    handler = NULL, action = NULL, container = NULL, ...,
    toolkit = guiToolkit())

  .ggraphics(toolkit, width = dpi<c2><a0>*<c2><a0>6,
    height = dpi<c2><a0>*<c2><a0>6, dpi = 75, ps = 12,
    handler = NULL, action = NULL, container = NULL, ...)
}
\arguments{
  \item{width}{width of device (pixels)}

  \item{height}{hieght of widget (pixels)}

  \item{dpi}{dots per inch}

  \item{ps}{pointsize}

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
  implementations.}

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}
}
\description{
  Some toolkits provide an embeddable graphics device. When
  this is the case, this widget provides same.

  generic for toolkit dispatch
}


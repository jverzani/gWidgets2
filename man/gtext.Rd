\name{gtext}
\alias{.gtext}
\alias{dispose.GText}
\alias{gtext}
\alias{insert}
\alias{insert.GText}
\title{Multiline text edit constructor}
\usage{
  gtext(text = NULL, width = NULL, height = 300, font.attr
  = NULL, wrap = TRUE, handler = NULL, action = NULL,
  container = NULL, ..., toolkit = guiToolkit())

  .gtext(toolkit, text = NULL, width = NULL, height = 300,
  font.attr = NULL, wrap = TRUE, handler = NULL, action =
  NULL, container = NULL, ...)

  insert(obj, value, where = c("end", "beginning",
  "at.cursor"), font.attr = NULL, do.newline = TRUE, ...)

  \method{insert}{GText} (obj, value, where = c("end",
  "beginning", "at.cursor"), font.attr = NULL, do.newline =
  TRUE, ...)

  \method{dispose}{GText} (obj, ...)
}
\arguments{
  \item{text}{initial text}

  \item{width}{width of widget}

  \item{height}{height of widget (when width is specified)}

  \item{font.attr}{font attributes for text buffer. One can
  also specify font attributes for insertion. The font
  attributes are specified with a named vector or list,
  with names and values coming from: \describe{
  \item{weight}{ in c("light", "normal", "bold", "heavy")}
  \item{style}{inc("normal", "oblique", "italic")}
  \item{family}{in c("sans", "helvetica", "times",
  "monospace")} \item{size}{in c("xx-small", "x-small",
  "small", "medium", "large", "x-large", "xx-large")}
  \item{foreground}{a value in colors()}
  \item{background}{a value in colors()} }}

  \item{wrap}{logical do lines wrap}

  \item{obj}{object}

  \item{value}{text to insert}

  \item{where}{position of insertion}

  \item{do.newline}{logical add a newline at end}

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
\value{
  called for side effect
}
\description{
  The multiline text widget has its main property the text
  contained within. \itemize{ \item{The \code{svalue} will
  return a string (length-1 character vector) with embedded
  newlines} \item{The "change" handler is
  \code{addHandlerKeystroke}} }

  generic for toolkit dispatch

  insert text into a gtext buffer

  insert method for gtext

  dispose method for gtext clears buffer
}


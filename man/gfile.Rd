\name{gfile}
\alias{.gfile}
\alias{.gfilebrowse}
\alias{gfile}
\alias{gfilebrowse}
\title{dialog for file and directory selection}
\usage{
  gfile(text = "", type = c("open", "save", "selectdir"),
    initial.filename = NULL, filter = list(),
    multi = FALSE, ..., toolkit = guiToolkit())

  .gfile(toolkit, text = "",
    type = c("open", "save", "selectdir"),
    initial.filename = NULL, filter = list(),
    multi = FALSE, ...)

  gfilebrowse(text = "Select a file...",
    type = c("open", "save", "selectdir"),
    initial.filename = NULL, filter = list(), quote = TRUE,
    handler = NULL, action = NULL, container = NULL, ...,
    toolkit = guiToolkit())

  .gfilebrowse(toolkit, text = "Select a file...",
    type = c("open", "save", "selectdir"),
    initial.filename = NULL, filter = list(), quote = TRUE,
    handler = NULL, action = NULL, container = NULL, ...)
}
\arguments{
  \item{text}{initial text}

  \item{type}{type of browser: to open a file, to save a
  file or to select a directory}

  \item{initial.filename}{Suggested file name}

  \item{filter}{A filter specifiation. This is toolkit
  specific. Here are some examples: \itemize{ \item{RGtk2}{
  Something like \preformatted{ list("All files" =
  list(patterns = c("*")), "R files" = list(patterns =
  c("*.R", "*.Rdata")), "text files" = list(mime.types =
  c("text/plain")) ) } } \item{tcltk}{} \item{Qt}{} }}

  \item{multi}{Logical. Allow multiple files to be
  selected?}

  \item{quote}{quote output}

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
}
\value{
  returns filename(s) or \code{character(0)} if no
  selection.

  Returns an object of class \code{gFilebrowse}. This
  should inherit the methods of \code{gedit} instances.
}
\description{
  dialog for file and directory selection

  generic for toolkit dispatch

  Basically an entry box instance with a button to initiate
  \code{gfile}.

  generic for toolkit dispatch
}


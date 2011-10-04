\name{gfile}
\alias{.gfile}
\alias{gfile}
\alias{gfilebrowse}
\title{dialog for file and directory selection}
\usage{
  gfile(text = "", type = c("open", "save", "selectdir"),
  initial.filename = NULL, filter = list(), multi = FALSE,
  ..., toolkit = guiToolkit())

  .gfile(toolkit, text = "", type = c("open", "save",
  "selectdir"), initial.filename = NULL, filter = list(),
  multi = FALSE, ...)

  gfilebrowse(text = "Select a file...", type = c("open",
  "save", "selectdir"), initial.filename = NULL, filter =
  list(), quote = TRUE, handler = NULL, action = NULL,
  container = NULL, ..., toolkit = guiToolkit())
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

  \item{...}{ignored}

  \item{toolkit}{toolkit}

  \item{text}{Instructional text}

  \item{type}{type of dialog (see \code{\link{gfile}})}

  \item{quote}{Do we quote value}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent}

  \item{toolkit}{toolkit}
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
}


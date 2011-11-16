\name{gedit}
\alias{.gedit}
\alias{gedit}
\title{Single line text edit constructor}
\usage{
  gedit(text = "", width = 25, coerce.with = NULL,
    initial.msg = "", handler = NULL, action = NULL,
    container = NULL, ..., toolkit = guiToolkit())

  .gedit(toolkit, text = "", width = 25,
    coerce.with = NULL, initial.msg = "", handler = NULL,
    action = NULL, container = NULL, ...)
}
\arguments{
  \item{text}{initial text}

  \item{width}{number of characters}

  \item{coerce.with}{A function or name of function to
  coerce value with before returning by \code{svalue}}

  \item{initial.msg}{If not text give, and an initial
  message given, then this message is displayed until the
  widget receives the focus}

  \item{handler}{Change handler. Called when return key is
  hit}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent}

  \item{toolkit}{toolkit}
}
\value{
  An object of class \code{GEdit}. This has sub-classed
  methods:

  \enumerate{

  \item \code{}

  \item \code{svalue} to retrieve the text

  \item \code{svalue<-} to set the text

  \item \code{[} to get the autocomplete values

  \item \code{[<-} Character. To set autocomplete values

  \item \code{visible<-} to specify a character to display
  instead of text (for passwords)

  }

  The default handler call is when the user activates the
  entry, typically by pressing the enter key.

  The \code{addhandlerBlur} method is called when the
  widget loses focuses.

  The \code{addHandlerKeystroke} method adds a handler
  called after each keystroke. If possible, the first
  argument has a component \code{key} passing back the last
  value.
}
\description{
  Single line text edit constructor

  generic for toolkit dispatch
}


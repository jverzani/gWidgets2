\name{gedit}
\alias{.gedit}
\alias{addHandlerChanged.GEdit}
\alias{gedit}
\alias{svalue.GEdit}
\title{Single line text edit constructor}
\usage{
  gedit(text = "", width = 25, coerce.with = NULL,
    initial.msg = "", handler = NULL, action = NULL,
    container = NULL, ..., toolkit = guiToolkit())

  .gedit(toolkit, text = "", width = 25,
    coerce.with = NULL, initial.msg = "", handler = NULL,
    action = NULL, container = NULL, ...)

  \method{addHandlerChanged}{GEdit} (obj, handler,
    action = NULL, ...)

  \method{svalue}{GEdit} (obj, index = NULL, drop = NULL,
    ...)
}
\arguments{
  \item{text}{initial text}

  \item{width}{number of characters}

  \item{coerce.with}{A function or name of function to
  coerce value with before returning by \code{svalue}}

  \item{initial.msg}{If no initial text is given but an
  initial message is, then this message is displayed until
  the widget receives the focus}

  \item{handler}{Change handler. Called when return key is
  hit}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent}

  \item{toolkit}{toolkit}

  \item{obj}{object receiving event and emitting a signal
  to the handler}

  \item{index}{NULL or logical. If \code{TRUE} and widget
  supports it an index, instead of a value will be
  returned.}

  \item{drop}{NULL or logical. If widget supports it, drop
  will work as it does in a data frame or perhaps someother
  means.}
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
}
\description{
  Single line text edit constructor

  generic for toolkit dispatch

  The default change handler call is when the user
  activates the entry, typically by pressing the enter key.
  Other possible events to consider are covered by:
  \code{addhandlerBlur} (when the widget loses focuses) and
  \code{addHandlerKeystroke} (called after each keystroke).
  For the latter, if the toolkit supports it, the handler's
  first argument has a component \code{key} passing back
  the keystroke information.

  The \code{svalue} method for a edit object refers to its
  main property, the text in the box,
}


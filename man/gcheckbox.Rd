\name{gcheckbox}
\alias{.gcheckbox}
\alias{[<-.GCheckbox}
\alias{gcheckbox}
\alias{svalue<-.GCheckbox}
\title{constructor for checkbox widget}
\usage{
  gcheckbox(text = "", checked = FALSE, use.togglebutton =
  FALSE, handler = NULL, action = NULL, container = NULL,
  ..., toolkit = guiToolkit())

  .gcheckbox(toolkit, text, checked = FALSE,
  use.togglebutton = FALSE, handler = NULL, action = NULL,
  container = NULL, ...)
}
\arguments{
  \item{text}{label text}

  \item{checked}{is button selected}

  \item{use.togglebutton}{Use a toggle button (shows
  depressed) not a check box}

  \item{handler}{Callback called when toggle is changed.}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of container}

  \item{toolkit}{toolkit}
}
\value{
  Returns an object of class \code{GCheckbox}.
}
\description{
  A checkbox widget is used to toggle the state of a
  labeled boolean variable. The main property of this
  widget is that state, not the label. This variable may be
  proxied in the usual way -- with a box that indicates or
  check if \code{TRUE} -- or through a toggle button.

  Generic for toolkit dispatch

  Ensure value is logical

  items assignment takes string
}
\note{
  The value is coerced to character, then only first
  element used for checkbox label
}


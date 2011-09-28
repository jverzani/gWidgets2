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

  \item{x}{GCheckbox instance}

  \item{i}{ignored}

  \item{j}{ignored}

  \item{...}{ignored}

  \item{value}{Value coerced to character, then only first
  element used for checkbox label}
}
\value{
  Returns an object of class \code{GCheckbox} for which the
  following methods are overridden: % \itemize{
  \item{\code{svalue} gets state, boolean value}

  \item{ \code{svalue<-} sets state by boolean value}

  \item{ \code{[} returns label}

  \item{ \code{[<-} sets label} }
}
\description{
  A checkbox widget has a checkbox or toggle button to
  indicate selection or not

  Generic for toolkit dispatch

  Ensure value is logical

  items assignment takes string
}


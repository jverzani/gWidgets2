\name{galert}
\alias{.galert}
\alias{galert}
\title{Alert dialog to display transient messages}
\usage{
  galert(msg, title = "message", delay = 3, parent = NULL, ..., toolkit = guiToolkit())

  .galert(toolkit, msg, title = "message", delay = 3, parent = NULL, ...)
}
\arguments{
  \item{msg}{character. main message. If length is 2,
  second component is used for detail, providing it is
  available.}

  \item{title}{Title (may not be displayed)}

  \item{delay}{length of time (in seconds) to display}

  \item{parent}{parent object to show near}

  \item{...}{ignored}

  \item{toolkit}{toolkit}
}
\description{
  Alert dialog to display transient messages

  generic for toolkit dispatch
}


\name{gmessage}
\alias{.gmessage}
\alias{gmessage}
\title{Constructor for modal message dialog}
\usage{
  gmessage(msg, title = "message", icon = c("info",
  "warning", "error", "question"), parent = NULL, ...,
  toolkit = guiToolkit())

  .gmessage(toolkit, msg = msg, title = title, icon = icon,
  parent = parent, ...)
}
\arguments{
  \item{msg}{Character. message to display.}

  \item{title}{Character. Title}

  \item{icon}{What icon to show}

  \item{parent}{Hint as to where to display}

  \item{...}{ignored}

  \item{toolkit}{toolkit}
}
\value{
  NULL

  NULL
}
\description{
  Constructor for modal message dialog

  generic for toolkit dispatch
}


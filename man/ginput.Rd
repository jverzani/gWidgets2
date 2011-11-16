\name{ginput}
\alias{.ginput}
\alias{ginput}
\title{Constructor for modal dialog to collect a line of text}
\usage{
  ginput(msg, text = "", title = "Input",
    icon = c("info",<c2><a0>"warning",<c2><a0>"error",<c2><a0>"question"),
    parent = NULL, ..., toolkit = guiToolkit())

  .ginput(toolkit, msg, text = "", title = "Input",
    icon = c("info",<c2><a0>"warning",<c2><a0>"error",<c2><a0>"question"),
    parent = NULL, ...)
}
\arguments{
  \item{msg}{Character. Message to display.}

  \item{text}{Character. Initial text}

  \item{title}{Character. Title of window}

  \item{icon}{which icon to display}

  \item{parent}{gives hint as to where to place dialog}

  \item{...}{ignored}

  \item{toolkit}{toolkit}
}
\value{
  value typed into box or \code{character(0)}
}
\description{
  Constructor for modal dialog to collect a line of text

  generic for toolkit dispatch
}


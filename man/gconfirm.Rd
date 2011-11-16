\name{gconfirm}
\alias{.gconfirm}
\alias{gconfirm}
\title{Constructor for modal dialog to get confirmation}
\usage{
  gconfirm(msg, title = "Confirm",
    icon = c("info",<c2><a0>"warning",<c2><a0>"error",<c2><a0>"question"),
    parent = NULL, ..., toolkit = guiToolkit())

  .gconfirm(toolkit, msg, title = "Confirm",
    icon = c("info",<c2><a0>"warning",<c2><a0>"error",<c2><a0>"question"),
    parent = NULL, ...)
}
\arguments{
  \item{msg}{Character. Message to display.}

  \item{title}{Character. Title of window}

  \item{icon}{which icon to display}

  \item{parent}{gives hint as to where to place dialog}

  \item{...}{ignored}

  \item{toolkit}{toolkit}
}
\value{
  logical inidicating confirmation
}
\description{
  Constructor for modal dialog to get confirmation

  generic for toolkit dispatch
}


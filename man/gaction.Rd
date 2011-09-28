\name{gaction}
\alias{.gaction}
\alias{gaction}
\title{##' An action constructor}
\usage{
  gaction(label, tooltip = NULL, icon = NULL, key.accel =
  NULL, handler = NULL, action = NULL, parent = NULL, ...,
  toolkit = guiToolkit())

  .gaction(toolkit, label, tooltip = NULL, icon = NULL,
  key.accel = NULL, handler = NULL, action = NULL, parent =
  NULL, ...)
}
\arguments{
  \item{label}{label for action}

  \item{tooltip}{toolktip for actin}

  \item{icon}{icon (stock icon name) for icon}

  \item{key.accel}{keyboard accelerator. If given, parent
  must be specified.}

  \item{handler}{handler to call when action is invoked}

  \item{action}{values passed to parameterize action}

  \item{parent}{parent window. Needed if keyboard
  accelerator used.}

  \item{...}{}

  \item{toolkit}{}
}
\value{
  a gaction instance
}
\description{
  ##' An action constructor

  generic for toolkit dispatch
}


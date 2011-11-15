\name{gaction}
\alias{.gaction}
\alias{gaction}
\title{An action constructor}
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

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations.}

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}
}
\value{
  a gaction instance
}
\description{
  A action object encapsulates an action (a callback)
  adding textual and graphic information. Actions may be
  proxied in buttons, menu bars or tool bars.

  generic for toolkit dispatch
}


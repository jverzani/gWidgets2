\name{gbasicdialog}
\alias{.gbasicdialog}
\alias{dispose.GBasicDialog}
\alias{gbasicdialog}
\alias{visible.GBasicDialog}
\title{Constructor for modal dialog that can contain an arbitrary widget}
\usage{
  gbasicdialog(title = "Dialog", parent = NULL, do.buttons
  = TRUE, handler = NULL, action = NULL, ..., toolkit =
  guiToolkit())

  .gbasicdialog(toolkit, title = "Dialog", parent = NULL,
  do.buttons = TRUE, handler = NULL, action = NULL, ...)

  visible.GBasicDialog(obj, ...)

  dispose.GBasicDialog(obj, ...)
}
\arguments{
  \item{title}{title for window}

  \item{parent}{parent to display by}

  \item{do.buttons}{FALSE to suppress buttons when no
  parent}

  \item{handler}{handler called when \code{Ok} button
  invoked}

  \item{action}{passed to handler for OK button}

  \item{...}{ignored}

  \item{toolkit}{toolkit}

  \item{obj}{gbasicdialog}

  \item{...}{}
}
\value{
  A \code{GBasicDialog} instance with a visible method

  logical indicating which button was pushed (or TRUE if
  not buttons present)

  NULL
}
\description{
  The basic dialog is basically a modal window. To use
  there is a 3 step process: 1) Create a container by
  calling this constructor, say \code{dlg}; 2) use
  \code{dlg} as a container for your subsequent GUI; 3) set
  the dialog to be modal by calling \code{visible(dlg)}.
  (One can't call \code{visible(dlg) <- TRUE}.)

  generic for toolkit dispatch

  The \code{gbasicdialog} container becomes visible and
  modal after this call.

  dispose of dialog
}


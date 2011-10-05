\name{gconfirm}
\alias{.gconfirm}
\alias{gconfirm}
\title{Constructor for modal dialog to get confirmation}
\usage{
  gconfirm(msg, title = "Confirm", icon = c("info",
  "warning", "error", "question"), parent = NULL, ...,
  toolkit = guiToolkit())

  .gconfirm(toolkit, msg, title = "Confirm", icon =
  c("info", "warning", "error", "question"), parent = NULL,
  ...)
}
\value{
  logical inidicating confirmation
}
\description{
  Constructor for modal dialog to get confirmation

  generic for toolkit dispatch
}


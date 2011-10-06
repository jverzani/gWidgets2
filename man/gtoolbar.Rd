\name{gtoolbar}
\alias{.gtoolbar}
\alias{add.GToolBar}
\alias{gtoolbar}
\title{A toolbar constructor}
\usage{
  gtoolbar(toolbar.list = list(), style = c("both",
  "icons", "text", "both-horiz"), container = NULL, ...,
  toolkit = guiToolkit())

  .gtoolbar(toolkit, toolbar.list = list(), style =
  c("both", "icons", "text", "both-horiz"), container =
  NULL, ...)

  add.GToolBar(obj, child, expand = FALSE, fill = NULL,
  anchor = NULL, ...)
}
\description{
  A toolbar constructor

  generic for toolkit dispatch

  add toolbar items to toolbar
}


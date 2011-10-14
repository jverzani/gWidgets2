\name{gtoolbar}
\alias{.gtoolbar}
\alias{add.GMenuBar}
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

  add.GMenuBar(obj, child, expand = FALSE, fill = NULL,
  anchor = NULL, ...)
}
\arguments{
  \item{toolbar.list}{list. A one-level list of
  \code{gaction} items, \code{gseparator} items or possibly
  other widgets. In the latter cases the \code{container}
  argument is not specified prior. (XXX Need to work this
  out with gWidgetstcltk)}

  \item{style}{style for icon or text.}

  \item{container}{a GWindow instance}

  \item{...}{ignored}

  \item{toolkit}{toolkit}

  \item{child}{list. a toolbar list (one-level named list
  of \code{gaction} or \code{gseparator} objects.)}

  \item{child}{list. a menubar list or gmenu instance.}

  \item{obj}{parent object}

  \item{expand}{NULL or logical. XXX}

  \item{fill}{NULL or logical. XXX}

  \item{anchor}{NULL or logical. XXX}
}
\description{
  A toolbar constructor

  generic for toolkit dispatch

  add toolbar items to toolbar

  add menubar items to a menu
}


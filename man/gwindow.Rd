\name{gwindow}
\alias{.gwindow}
\alias{add.GWindow}
\alias{dispose.GWindow}
\alias{gwindow}
\title{gwindow}
\usage{
  gwindow(title = "Window", visible = TRUE, name = title,
  width = NULL, height = NULL, parent = NULL, handler =
  NULL, action = NULL, ..., toolkit = guiToolkit())

  .gwindow(toolkit, title, visible, name, width, height,
  parent, handler, action, ...)

  add.GWindow(obj, child, expand = NULL, fill = NULL,
  anchor = NULL, ...)

  dispose.GWindow(obj, ...)
}
\arguments{
  \item{title}{title for window's title bar}

  \item{visible}{logical. If code{TRUE} window is drawn
  when constructed. Otherwise, window can be drawn later
  using \code{visible<-}. This value can default to
  \code{FALSE} by setting the option:
  \code{options("gWidgets:gwindow-default-visible-is-false"=TRUE)}.
  There are advantages: windows can draw slowly when adding
  item by ‘ggraphics’ like to be added to an undrawn
  widget as this avoids sizing issue.}

  \item{name}{Name for registry of windows}

  \item{width}{initial width of window}

  \item{height}{initial height of window. This sets height
  before window manager manages the window}

  \item{parent}{If non-NULL, can be used to suggest default
  location of window. The argument name was changed from
  location to parent. This can be a coordinate pair (x,y)
  with (0,0) the upper left corner, or a gwindow instance.
  In the latter case the location is suggested by the
  location of the current window. This is useful for
  placing dialogs near the parent window.}

  \item{handler}{handler for destroy event}

  \item{action}{action passed t handler}

  \item{...}{ignored}

  \item{toolkit}{toolkit}

  \item{obj}{\code{gwindow} object}

  \item{child}{child object to add}

  \item{expand}{ignored}

  \item{fill}{ingnored}

  \item{anchor}{ignored}

  \item{...}{ignored}

  \item{obj}{toplevel window object}

  \item{...}{ignored}
}
\value{
  a \code{GWindow} instance
}
\description{
  top-level window object

  S3 generic whose methods are implemented in the toolkit
  packages

  Dispatches on type of child (menubar, toolbar, widget)

  dispose method for gwindow
}
\author{
  john verzani
}


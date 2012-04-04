\name{gwindow}
\alias{.gwindow}
\alias{add.GWindow}
\alias{dispose.GWindow}
\alias{gwindow}
\title{gwindow}
\usage{
  gwindow(title = "Window", visible = TRUE, name = title,
    width = NULL, height = NULL, parent = NULL,
    handler = NULL, action = NULL, ...,
    toolkit = guiToolkit())

  .gwindow(toolkit, title, visible, name, width, height,
    parent, handler, action, ...)

  \method{add}{GWindow} (obj, child, expand = NULL,
    fill = NULL, anchor = NULL, ...)

  \method{dispose}{GWindow} (obj, ...)
}
\arguments{
  \item{title}{title for window's title bar. This is the
  main property and is accessed via \code{svalue} or
  \code{svalue<-}.}

  \item{visible}{logical. If code{TRUE} window is drawn
  when constructed. Otherwise, window can be drawn later
  using \code{visible<-}. This value can default to
  \code{FALSE} by setting the option:
  \code{options("gWidgets:gwindow-default-visible-is-false"=TRUE)}.
  There are advantages: windows can draw slowly when adding
  many items. With \pkg{gWidgets2RGtk2}, the
  \code{ggraphics} widget can like to be added to an
  undrawn widget as this avoids sizing issue.}

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

  \item{obj}{parent object}

  \item{child}{child widget}

  \item{expand}{NULL or logical. XXX}

  \item{fill}{NULL or logical. XXX}

  \item{anchor}{NULL or logical. XXX}
}
\value{
  a \code{GWindow} instance
}
\description{
  top-level window object

  S3 generic whose methods are implemented in the toolkit
  packages

  Dispatches on type of child (menubar, toolbar, statusbar,
  widget)

  The \code{dispose} method destroys the top-level window
  and its children.
}
\author{
  john verzani
}


\name{gmenu}
\alias{.gmenu}
\alias{gmenu}
\title{menu constructor, main and popup}
\usage{
  gmenu(menu.list, popup = FALSE, container = NULL, ...,
  toolkit = guiToolkit())

  .gmenu(toolkit, menu.list = list(), popup = FALSE,
  container = NULL, ...)
}
\arguments{
  \item{menu.list}{A list defining the menu structure.
  Named sub lists determine the submenu titles and
  structure. The list may have components of class:
  \code{GAction}, mapped to a button; \code{GSeparator},
  mapped to a horizontal separator; \code{GRadio}, mapped
  to linked buttons; or \code{GCheckbox}, mapped to a
  checkbox button.}

  \item{popup}{logical. If true, make a popup window to be
  added through a handler call}

  \item{container}{For non-popup menus, a top-level
  \code{GWindow} instance to place the menu bar within.}

  \item{...}{}

  \item{toolkit}{}
}
\description{
  A menu may be viewed as a heirarchical collection of
  buttons, each invoked by clicking on the button. These
  buttons are exposed through submenus. More generally, a
  widget may replace the button. This widget intends to
  support buttons (gactions), separators (gseparator),
  radio button (gradio) and checkbutton (gcheckbox).

  generic for toolkit dispatch
}


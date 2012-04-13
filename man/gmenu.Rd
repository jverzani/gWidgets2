\name{gmenu}
\alias{.gmenu}
\alias{add.GMenuBar}
\alias{gmenu}
\alias{svalue.GMenuBar}
\alias{svalue<-.GMenuBar}
\title{menu constructor, main and popup}
\usage{
  gmenu(menu.list, popup = FALSE, container = NULL, ..., toolkit = guiToolkit())

  .gmenu(toolkit, menu.list = list(), popup = FALSE, container = NULL, ...)

  \method{add}{GMenuBar} (obj, child, expand = FALSE, fill = NULL, anchor = NULL, ...)

  \method{svalue}{GMenuBar} (obj, index = NULL, drop = NULL, ...)

  \method{svalue}{GMenuBar} (obj, index = NULL, ..., value) <- value
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

  \item{child}{list. a menubar list or gmenu instance.}

  \item{index}{ignored}

  \item{drop}{ignored}

  \item{value}{a list or menu bar specifying the new men
  ubar}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations. For example, when using a widget as a
  menubar object one can specify a parent argument to pass
  in parent information, similar to how the argument is
  used with gaction and the dialogs.}

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}

  \item{obj}{parent object}

  \item{expand}{NULL or logical. XXX}

  \item{fill}{NULL or logical. XXX}

  \item{anchor}{NULL or logical. XXX}
}
\description{
  A menu may be viewed as a heirarchical collection of buttons, each invoked by clicking on the button. These buttons are exposed through submenus. More generally, a widget may replace the button. This widget intends to support buttons (gactions), separators (gseparator), radio button (gradio) and checkbutton (gcheckbox), but this may be toolkit independent. When using a radio button or checkbox, one should pass in a \code{parent} argument to the constructor -- not a \code{container}.

  generic for toolkit dispatch

  add menubar items to a menu

  For a menubar, \code{svalue} returns the list of action items etc. that defined the menubar. This can be useful to access the underlying item being proxied. (For \code{gaction} items the \code{enabled<-} method may be used on the item, but this may not extend to \code{gradio} and \code{gcheckbox} items)

  for a menubar, \code{svalue<-} replaces the menubar items with new ones specified by value.
}


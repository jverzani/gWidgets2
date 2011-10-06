\name{getToolkitWidget}
\alias{getBlock}
\alias{getBlock.GComponent}
\alias{getBlock.GWindow}
\alias{getToolkitWidget}
\alias{getToolkitWidget.default}
\alias{getTopLevel}
\alias{getTopLevel.GComponent}
\alias{getWidget}
\alias{getWidget.GComponent}
\title{Get underlying toolkit widget}
\usage{
  getToolkitWidget(obj)

  getToolkitWidget.default(obj)

  getWidget(obj)

  getWidget.GComponent(obj)

  getBlock(obj)

  getBlock.GComponent(obj)

  getBlock.GWindow(obj)

  getTopLevel(obj)

  getTopLevel.GComponent(obj)
}
\arguments{
  \item{obj}{object}

  \item{obj}{object}
}
\description{
  At times a user may wish to access the underlying toolkit
  widget. Although this is not cross-platform, one often
  has access to many more methods of the object, than
  through those provided by gWidgets.

  Basic S3 method

  Get underlying toolkit widget from widget slot. Used
  internally

  method for getWidget

  Get underlying toolkit object from block slot

  S3 method for getBlock generic

  For GWindow, the block is NULL

  Get toplevel window containing object

  getTopLevel method for components
}


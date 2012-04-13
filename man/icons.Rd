\name{addStockIcons}
\alias{.addStockIcons}
\alias{.addStockIcons.default}
\alias{.getStockIconByName}
\alias{.getStockIconByName.default}
\alias{.getStockIcons}
\alias{.getStockIcons.default}
\alias{.stockIconFromClass}
\alias{.stockIconFromClass.default}
\alias{.stockIconFromObject}
\alias{.stockIconFromObject.default}
\alias{addStockIcons}
\alias{getStockIconByName}
\alias{getStockIcons}
\alias{stockIconFromClass}
\alias{stockIconFromObject}
\title{Method to add icon to list of stock icons}
\usage{
  addStockIcons(iconNames, iconFiles, ..., toolkit = guiToolkit())

  .addStockIcons(toolkit, iconNames, iconFiles, ...)

  \method{.addStockIcons}{default} (toolkit, iconNames, iconFiles, ...)

  getStockIcons(..., toolkit = guiToolkit())

  .getStockIcons(toolkit, ...)

  \method{.getStockIcons}{default} (toolkit, ...)

  getStockIconByName(name, ..., toolkit = guiToolkit())

  .getStockIconByName(toolkit, name, ...)

  \method{.getStockIconByName}{default} (toolkit, name, file = TRUE, ...)

  stockIconFromClass(theClass, ..., toolkit = guiToolkit())

  .stockIconFromClass(toolkit, theClass, ...)

  \method{.stockIconFromClass}{default} (toolkit, theClass, ...)

  stockIconFromObject(obj, ..., toolkit = guiToolkit())

  .stockIconFromObject(toolkit, obj, ...)

  \method{.stockIconFromObject}{default} (toolkit, obj, ...)
}
\arguments{
  \item{iconNames}{names of icons}

  \item{iconFiles}{path of icons}

  \item{...}{ignored}

  \item{toolkit}{used to dispatch into toolkit if a
  separate implementation is made}

  \item{name}{of stock icon}

  \item{file}{logical If TRUE, return filename. If FALSE,
  return toolkit icon object (if possible).}

  \item{theClass}{name of class}

  \item{obj}{an R object}
}
\value{
  list of icons with names the icon name and values the icon file name or icon object (as needed by the toolkit)

  name of icon.
}
\description{
  Method to add icon to list of stock icons

  generic for dispath

  toolkit implementation

  return list of available stock icons

  generic for toolkit dispatch

  default

  Return stock icon name, filename, icon object from its by name

  generic

  default implementation

  Find a stock icon from the given class

  generic for dispath

  Default stock icon for a given class name

  Find stock icon from the given object

  generic for dispath

  get stock icon from object by class
}


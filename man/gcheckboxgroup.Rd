\name{gcheckboxgroup}
\alias{.gcheckboxgroup}
\alias{gcheckboxgroup}
\title{Constructor for checkbox group. A linked group of checkboxes, but not exclusive.}
\usage{
  gcheckboxgroup(items, checked = FALSE, horizontal =
  FALSE, use.table = FALSE, handler = NULL, action = NULL,
  container = NULL, ..., toolkit = guiToolkit())

  .gcheckboxgroup(toolkit, items, checked = FALSE,
  horizontal = FALSE, handler = NULL, action = NULL,
  container = NULL, ...)
}
\arguments{
  \item{items}{checkbox labels}

  \item{checked}{logical. Are values checked}

  \item{horizontal}{logical. If true displayed
  horizontally, else vertically}

  \item{use.table}{logical. If supported, and \code{TRUE}
  then use a table widget with scrollbars}

  \item{handler}{Handler called when state toggles}

  \item{action}{passed to handler when called}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent}

  \item{toolkit}{toolkit}
}
\value{
  Returns an object of class \code{GCheckboxGroup} for
  which the following methods are overridden: \itemize{
  \item{ \code{svalue} Return the selected values or an
  empty character vector. If \code{index=TRUE}, returns
  indices of selected values.} \item{ \code{svalue<-} Set
  the selected values one of three ways: by label name, by
  a logical variable indicating which are selected (if
  ambigous, logical wins), if \code{index=TRUE} by the
  indices to select.} \item{ \code{[} returns labels}
  \item{ \code{[<-} set the label values. Should be able to
  shorten or lengthen list} }
}
\description{
  Constructor for checkbox group. A linked group of
  checkboxes, but not exclusive.

  generic for toolkit dispatch
}


\name{gcalendar}
\alias{.gcalendar}
\alias{gcalendar}
\title{A constructor for a date selection widget}
\usage{
  gcalendar(text = "", format = "\%Y-\%m-\%d",
    handler = NULL, action = NULL, container = NULL, ...,
    toolkit = guiToolkit())

  .gcalendar(toolkit, text = "", format = "\%Y-\%m-\%d",
    handler = NULL, action = NULL, container = NULL, ...)
}
\arguments{
  \item{text}{initial text}

  \item{format}{Date format}

  \item{handler}{handler called when changed}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent}

  \item{toolkit}{toolkit}
}
\value{
  Returns an object of class \code{GCalendar} for which the
  following methods are overridden: \enumerate{ \item
  \code{svalue} get the date

  \item \code{svalue<-} set the date } The change handler
  is inherited from \code{\link{gedit}}
}
\description{
  The date is the main property of this widget

  generic for toolkit dispatch
}


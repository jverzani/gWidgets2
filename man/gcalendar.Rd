\name{gcalendar}
\alias{.gcalendar}
\alias{addHandlerChanged.GCalendar}
\alias{gcalendar}
\alias{svalue.GCalendar}
\title{A constructor for a date selection widget}
\usage{
  gcalendar(text = "", format = "\%Y-\%m-\%d", handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())

  .gcalendar(toolkit, text = "", format = "\%Y-\%m-\%d", handler = NULL, action = NULL, container = NULL, ...)

  \method{addHandlerChanged}{GCalendar} (obj, handler, action = NULL, ...)

  \method{svalue}{GCalendar} (obj, index = NULL, drop = NULL, ...)
}
\arguments{
  \item{text}{initial text}

  \item{format}{Date format}

  \item{handler}{handler called when changed}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of parent}

  \item{toolkit}{toolkit}

  \item{obj}{receiver object}

  \item{index}{ignored}

  \item{drop}{if \code{TRUE} return a character, else a
  \code{Date} class object.}
}
\value{
  Returns an object of class \code{GCalendar} for which the following methods are overridden: \enumerate{ \item \code{svalue} get the date

  \item \code{svalue<-} set the date } The change handler is inherited from \code{\link{gedit}}

  If \code{drop=TRUE} a character string, else a \code{Date} class object.
}
\description{
  The date is the main property of this widget

  generic for toolkit dispatch

  Change handler for a calendar is called when the text value is updated

  The \code{svalue} method for a calendar object returns the selected date
}


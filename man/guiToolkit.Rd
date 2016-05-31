% Please edit documentation in R/guiToolkit.R
\name{guiToolkit}
\alias{guiToolkit}
\title{set or get the current toolkit for gWidgets}
\usage{
guiToolkit(name = NULL)
}
\arguments{
\item{name}{name of toolkit (e.g. "tcltk", "RGtk2", "Qt" (not
qtbase)). If NULL, then we search for it in a) an iherited toolkit
object b) the "guiToolkit" option (which can be set via
\code{options("guiToolkit"="RGtk2")}, say. If that fails, we
prompt for a selection for any installed toolkit.  In the typical
usage, this all happens in the background, except perhaps once.

In design this is to allow different toolkits to be used with
different GUIs, but due to differences in event loops, this often
leads to lockups, so is not recommended.}
}
\value{
an instance of guiWidgetsToolkit sub class.
}
\description{
set or get the current toolkit for gWidgets
}


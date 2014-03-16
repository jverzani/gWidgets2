\name{gbasicdialog}
\alias{.gbasicdialog}
\alias{dispose.GBasicDialog}
\alias{gbasicdialog}
\alias{visible.GBasicDialog}
\title{Constructor for modal dialog that can contain an arbitrary widget}
\usage{
  gbasicdialog(title = "Dialog", parent = NULL,
    do.buttons = TRUE, handler = NULL, action = NULL, ...,
    toolkit = guiToolkit())

  .gbasicdialog(toolkit, title = "Dialog", parent = NULL,
    do.buttons = TRUE, handler = NULL, action = NULL, ...)

  \method{visible}{GBasicDialog} (obj, ...)

  \method{dispose}{GBasicDialog} (obj, ...)
}
\arguments{
  \item{title}{title for window}

  \item{parent}{parent to display by}

  \item{do.buttons}{FALSE to suppress buttons when no
  parent}

  \item{handler}{handler called when \code{Ok} button
  invoked}

  \item{action}{passed to handler for OK button}

  \item{...}{ignored}

  \item{toolkit}{toolkit}

  \item{obj}{dialog object}
}
\value{
  A \code{GBasicDialog} instance with a visible method

  logical indicating which button was pushed (or TRUE if no
  buttons present)

  NULL
}
\description{
  The basic dialog is basically a modal window. To use
  there is a 3 step process: 1) Create a container by
  calling this constructor, say \code{dlg}; 2) use
  \code{dlg} as a container for your subsequent GUI; 3) set
  the dialog to be modal by calling \code{visible(dlg)}.
  (One can't call \code{visible(dlg) <- TRUE}.)

  generic for toolkit dispatch

  We overrided the basic use of \code{visible} for the
  \code{gbasicdialog} container to have it become visible
  and modal after this call. The better suited call
  \code{visible(dlg) <- TRUE} does not work as wanted, for
  we want to capture the return value.

  dispose method for a basic dialog
}
\examples{
\dontrun{
## a modal dialog for editing a data frme
fix_df <- function(DF, ...) {
  dfname <- deparse(substitute(DF))
  w <- gbasicdialog(..., handler=function(h,...) {
    assign(dfname, df[,], .GlobalEnv)
  })
  g <- ggroup(cont=w, horizontal=FALSE)
  glabel("Edit a data frame", cont=g)
  df <- gdf(DF, cont=g, expand=TRUE)
  size(w) <- c(400, 400)
  out <- visible(w)
}

m <- mtcars[1:3, 1:4]
fix_df(m)
}
}


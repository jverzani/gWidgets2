\name{gformlayout}
\alias{gformlayout}
\title{A form layout container}
\usage{
  gformlayout(align = c("left", "center"), spacing = 5,
  container = NULL, ..., toolkit = guiToolkit())
}
\arguments{
  \item{align}{alignment of label. Left justify or center
  balance.}

  \item{spacing}{spacing between columns}
}
\description{
  This convenience container is basically a simpler form of
  \code{glayout} to be used to layout two columns forms
  with a label on the left.  The label can be passed in to
  the \code{add} method of the container as is done with
  notebook labels
}


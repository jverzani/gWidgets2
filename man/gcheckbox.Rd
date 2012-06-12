\name{gcheckbox}
\alias{.gcheckbox}
\alias{[<-.GCheckbox}
\alias{addHandlerChanged.GCheckbox}
\alias{gcheckbox}
\alias{svalue<-.GCheckbox}
\title{constructor for checkbox widget}
\usage{
  gcheckbox(text = "", checked = FALSE,
    use.togglebutton = FALSE, handler = NULL,
    action = NULL, container = NULL, ...,
    toolkit = guiToolkit())

  .gcheckbox(toolkit, text, checked = FALSE,
    use.togglebutton = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)

  \method{addHandlerChanged}{GCheckbox}(obj, handler,
    action = NULL, ...)

  \method{svalue}{GCheckbox}(obj, index = NULL, ...) <-
    value

  \method{[}{GCheckbox}(x, i, j, ...) <- value
}
\arguments{
  \item{text}{label text}

  \item{checked}{is button selected}

  \item{use.togglebutton}{Use a toggle button (shows
  depressed) not a check box}

  \item{handler}{Callback called when toggle is changed.}

  \item{action}{passed to handler}

  \item{container}{parent container}

  \item{...}{passed to \code{add} method of container}

  \item{toolkit}{toolkit}

  \item{obj}{receiver object}

  \item{index}{ignored. Input is coerced to logical.}

  \item{value}{assignment value}

  \item{x}{checkbox object}

  \item{i}{item index}

  \item{j}{ignored}
}
\value{
  Returns an object of class \code{GCheckbox}.
}
\description{
  A checkbox widget is used to toggle the state of a
  labeled boolean variable. The main property of this
  widget is that state, not the label. This variable may be
  proxied in the usual way -- with a box that indicates or
  check if \code{TRUE} -- or through a toggle button.

  Generic for toolkit dispatch

  The change handler for \code{GCheckbox} is called when
  the value toggles. You can inpsect the current value in
  the callback to have an action based on the state.

  The \code{svalue<-} method ensures the value is a logical
  vector of length 1.

  items assignment takes string
}
\note{
  The value is coerced to character, then only first
  element used for checkbox label
}
\examples{
if(interactive()) {
  w <- gwindow("Selection widgets")
  g <- gvbox(cont=w)

  fl <- gformlayout(cont=g)
  gcheckbox("checkbox", checked=TRUE, cont=fl, label="checkbox")
  gradio(state.name[1:4], selected=2, horizontal=TRUE, cont=fl, label="gradio")
  gcheckboxgroup(state.name[1:4], horizontal=FALSE, cont=fl, label="checkbox group")

  bg <- ggroup(cont=g)
  gbutton("ok", cont=bg, handler=function(h,...) print(sapply(fl$children, svalue)))



}
}


\name{gcheckbox}
\alias{.gcheckbox}
\alias{[<-.GCheckbox}
\alias{addHandlerChanged.GCheckbox}
\alias{gcheckbox}
\alias{svalue<-.GCheckbox}
\title{constructor for checkbox widget}
\usage{
  gcheckbox(text = "", checked = FALSE, use.togglebutton = FALSE, handler = NULL, action = NULL, container = NULL, ..., toolkit = guiToolkit())

  .gcheckbox(toolkit, text, checked = FALSE, use.togglebutton = FALSE, handler = NULL, action = NULL, container = NULL, ...)

  \method{addHandlerChanged}{GCheckbox} (obj, handler, action = NULL, ...)

  \method{svalue}{GCheckbox} (obj, index = NULL, ..., value) <- value

  [.GCheckbox(x, i, j, ..., value) <- value
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

  \item{value}{typically an array-like \R object of a
  similar class as \code{x}.}
}
\value{
  Returns an object of class \code{GCheckbox}.
}
\description{
  A checkbox widget is used to toggle the state of a labeled boolean variable. The main property of this widget is that state, not the label. This variable may be proxied in the usual way -- with a box that indicates or check if \code{TRUE} -- or through a toggle button.

  Generic for toolkit dispatch

  The change handler for \code{GCheckbox} is called when the value toggles. You can inpsect the current value in the callback to have an action based on the state.

  The \code{svalue<-} method ensures the value is a logical vector of length 1.

  items assignment takes string
}
\note{
  The value is coerced to character, then only first element used for checkbox label
}
\examples{
if(interactive()) {
  ## a tour of the selection widgets

  w <- gwindow("Selection widgets")
  lyt <- glayout(cont=w)

  ## checkbox
  lyt[1,1] <- "checkbox"
  lyt[1,2] <- gcheckbox("checkbox", checked=FALSE, cont=lyt)

  ## radio button group
  lyt[2,1] <- "radio buttons"
  lyt[2,2] <- gradio(state.name[1:3], selected=2, cont=lyt, horizontal=TRUE)
  
  ## checkbox group
  lyt[3,1] <- "checkbox group"
  lyt[3,2] <- gcheckboxgroup(state.name[1:3], checked=c(TRUE, FALSE, TRUE), cont=lyt, horizontal=TRUE)
  
  ## checkbox group, using atable
  lyt[4,1] <- "checkbox group"
  lyt[4,2] <- gcheckboxgroup(state.name[1:3], checked=c(TRUE, FALSE, TRUE), cont=lyt, use.table=TRUE)
  
  ## combobox (drop list)
  lyt[5,1] <- "combobox"
  lyt[5,2] <- gcombobox(state.name, selected=match("New York", state.name), cont=lyt)
  
  
  ## editable combobox
  lyt[6,1] <- "editable combobox"
  lyt[6,2] <- gcombobox(state.name, selected=match("New York", state.name), cont=lyt, editable=TRUE)

  lyt[7,1:2] <- gseparator(cont=lyt)

  lyt[8,2] <- gbutton("values", cont=lyt, handler=function(h,...) {
    print(sapply(lyt[1:6, 2], svalue))
  })
                      
  visible(w) <- TRUE
}
}


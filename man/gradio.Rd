\name{gradio}
\alias{.gradio}
\alias{[<-.GRadio}
\alias{gradio}
\alias{svalue.GRadio}
\title{Constructor for radio button widget}
\usage{
  gradio(items, selected = 1, horizontal = FALSE,
    handler = NULL, action = NULL, container = NULL, ...,
    toolkit = guiToolkit())

  .gradio(toolkit, items, selected = 1, horizontal = FALSE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{svalue}{GRadio} (obj, index = NULL, drop = TRUE,
    ...)

  \method{[}{GRadio} (x, i, j, ..., value) <- value
}
\arguments{
  \item{items}{items to select from}

  \item{selected}{index of initially selected item}

  \item{horizontal}{layout direction}

  \item{handler}{A handler assigned to the default change
  signal. Handlers are called when some event triggers a
  widget to emit a signal. For each widget some default
  signal is assumed, and handlers may be assigned to that
  through \code{addHandlerChanged} or at construction time.
  Handlers are functions whose first argument, \code{h} in
  the documentation, is a list with atleast two components
  \code{obj}, referring to the object emitting the signal
  and \code{action}, which passes in user-specified data to
  parameterize the function call.}

  \item{action}{User supplied data passed to the handler
  when it is called}

  \item{container}{A parent container. When a widget is
  created it can be incorporated into the widget heirarchy
  by passing in a parent container at construction time.
  (For some toolkits this is not optional, e.g.
  \pkg{gWidgets2tcltk} or \pkg{gWidgets2WWW2}.)}

  \item{...}{These values are passed to the \code{add}
  method of the parent container, and occasionally have
  been used to sneak in hidden arguments to toolkit
  implementations.}

  \item{toolkit}{Each widget constructor is passed in the
  toolkit it will use. This is typically done using the
  default, which will lookup the toolkit through
  \code{\link{guiToolkit}}.}

  \item{obj}{object of method call}

  \item{index}{NULL or logical. If \code{TRUE} and widget
  supports it an index, instead of a value will be
  returned.}

  \item{drop}{NULL or logical. If widget supports it, drop
  will work as it does in a data frame or perhaps someother
  means.}
}
\description{
  The radio button widget shows 2 or more items forcing the
  user to select one.

  Generic for method dispatch

  For a radio button group, with \code{svalue} the value
  can be referred to by index or label. Similarly with
  setting of a value

  Check for repeated items before passing on to
  \code{set_items}
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


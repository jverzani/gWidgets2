\name{gcheckboxgroup}
\alias{.gcheckboxgroup}
\alias{gcheckboxgroup}
\title{Constructor for checkbox group. A linked group of checkboxes, but not exclusive.}
\usage{
  gcheckboxgroup(items, checked = FALSE,
    horizontal = FALSE, use.table = FALSE, handler = NULL,
    action = NULL, container = NULL, ...,
    toolkit = guiToolkit())

  .gcheckboxgroup(toolkit, items, checked = FALSE,
    horizontal = FALSE, use.table = FALSE, handler = NULL,
    action = NULL, container = NULL, ...)
}
\arguments{
  \item{items}{checkbox labels}

  \item{checked}{logical. Are values checked}

  \item{horizontal}{logical. If true displayed
  horizontally, else vertically}

  \item{use.table}{logical. If supported, and \code{TRUE}
  then uses a table widget with scrollbars}

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
}
\value{
  Returns an object of class \code{GCheckboxGroup} for
  which the following methods are overridden:

  \itemize{

  \item{ \code{svalue} Return the selected values or an
  empty character vector. If \code{index=TRUE}, returns
  indices of selected values.}

  \item{ \code{svalue<-} Set the selected values one of
  three ways: by label name, by a logical variable
  indicating which are selected (if ambigous, logical
  wins), if \code{index=TRUE} by the indices to select.}

  \item{ \code{[} returns labels}

  \item{ \code{[<-} set the label values. Should be able to
  shorten or lengthen list}

  }
}
\description{
  Constructor for checkbox group. A linked group of
  checkboxes, but not exclusive.

  generic for toolkit dispatch
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


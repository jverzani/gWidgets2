\name{gradio}
\alias{.gradio}
\alias{[<-.GRadio}
\alias{gradio}
\alias{svalue.GRadio}
\alias{svalue<-.GRadio}
\title{Constructor for radio button widget}
\usage{
  gradio(items, selected = 1, horizontal = FALSE,
    handler = NULL, action = NULL, container = NULL, ...,
    toolkit = guiToolkit())

  .gradio(toolkit, items, selected = 1, horizontal = FALSE,
    handler = NULL, action = NULL, container = NULL, ...)

  \method{svalue}{GRadio}(obj, index = NULL, drop = TRUE,
    ...)

  \method{svalue}{GRadio}(obj, index = NULL, drop = TRUE,
    ..., value) <- value

  \method{[}{GRadio}(x, i, j, ..., value) <- value
}
\arguments{
  \item{items}{items to select from}

  \item{selected}{index of initially selected item}

  \item{horizontal}{layout direction}

  \item{x}{\code{GRadio} object}

  \item{i}{button index. Leavel as missing to replace items
  to select from.}

  \item{j}{ignored}

  \item{value}{items to assigns a choices for the buttons}

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
  implementations. For example, when using a widget as a
  menubar object one can specify a parent argument to pass
  in parent information, similar to how the argument is
  used with gaction and the dialogs.}

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

  The svalue method returns the radio button label or its
  index if \code{index=TRUE}. Labels are coerced to
  character by many of the toolkits. To be sure to return a
  numeric value, one can assign to the \code{coerce_with}
  property, e.g., \code{obj$coerce_with <- as.numeric}. For
  all widgets, if a function is specified to
  \code{coerce_with} it will be called on the value
  returned by \code{svalue}.

  For a radio button group, with \code{svalue} the value
  can be referred to by index or label.

  Check for repeated items before passing on to
  \code{set_items}
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


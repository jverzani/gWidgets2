\name{ggroup}
\alias{.ggroup}
\alias{ggroup}
\alias{gvbox}
\alias{svalue<-.GGroup}
\title{Basic box container}
\usage{
  ggroup(horizontal = TRUE, spacing = 5,
    use.scrollwindow = FALSE, container = NULL, ...,
    toolkit = guiToolkit())

  .ggroup(toolkit, horizontal = TRUE, spacing = 5,
    use.scrollwindow = FALSE, container = NULL, ...)

  \method{svalue}{GGroup} (obj, index = TRUE, ..., value)
    <- value

  gvbox(spacing = 5, use.scrollwindow = FALSE,
    container = NULL, ..., toolkit = guiToolkit())
}
\arguments{
  \item{horizontal}{logical. If TRUE, left to right layout,
  otherwise top to bottom}

  \item{spacing}{spacing aroud widget}

  \item{use.scrollwindow}{logical. If TRUE, places box
  container in scrollwindow. In this case, a size should be
  set.}

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
}
\value{
  a GGroup instance.

  a GGroup instance with vertical packing.
}
\description{
  Basic box container

  S3 generic whose methods are implemented in the toolkit
  packages

  The \code{svalue} method refers to the main property of
  the box container, its spacing. There are generally two
  types of spacing: padding around border of the box and
  spacing between each child that is packed in. The spacing
  here is the between-child-component spacing. The
  reference class method \code{set_borderwidth} can be used
  for the other.

  Avoids need to type \code{horizontal=FALSE}
}
\examples{
if(interactive()) {

  ## basic container:
  w <- gwindow("Box container examples", visible=FALSE)
  g <- ggroup(cont=w, horizontal=FALSE)

  ## simple horizontal box
  g1 <- ggroup(cont=g)
  sapply(c("one", "two", "three"), function(i) gbutton(i, cont=g1))

  ## framed box
  g2 <- gframe("Framed box", cont=g)
  sapply(c("one", "two", "three"), function(i) gbutton(i, cont=g2))

  ## expanding box
  g3 <- gexpandgroup("expanding box", cont=g)
  sapply(c("one", "two", "three"), function(i) gbutton(i, cont=g3))
  visible(g3) <- TRUE


  ## visible
  g5 <- ggroup(cont=g)
  l <- glabel("click button to hide/show label", cont=g5)
  gbutton("hide/show label", cont=g5, handler=function(h,...) {
    l <- g5[1]
    visible(l) <- !visible(l)
  })

  ## delete
  g6 <- ggroup(cont=g)
  l <- glabel("click button to delete label", cont=g6)
  gbutton("delete label", cont=g6, handler=function(h,...) {
    delete(g6, l)
    enabled(h$obj) <- FALSE
  })

  ## parent, child
  identical(l$parent[1], l)             # true
  

  visible(w) <- TRUE


  ## anchor, expand, fill
  w <- gwindow("expand, anchor, fill", visible=FALSE)
  g <- ggroup(cont=w, horizontal=FALSE)

  ## no expand
  g1 <- ggroup(cont=g, expand=TRUE)
  glabel("no expand", cont=g1)

  ## no expand
  g1 <- ggroup(cont=g, expand=TRUE)

  ## expand, anchor
  g1 <- ggroup(cont=g, expand=TRUE)
  b1 <- glabel("expand, c(-1,1)", cont=g1, expand=TRUE, anchor=c(-1,1), fill=FALSE)
  
  ## expand, fill=TRUE
  g1 <- ggroup(cont=g, expand=TRUE)
  b1 <- glabel("expand=fill=TRUE", cont=g1, expand=TRUE, fill=TRUE)

  ## expand, fill=x
  g1 <- ggroup(cont=g, expand=TRUE)
  b1 <- glabel("expand=TRUE, fill='x'", cont=g1, expand=TRUE, fill="x")


  ## expand, fill=y
  g1 <- ggroup(cont=g, expand=TRUE)
  b1 <- glabel("expand=TRUE, fill='y'", cont=g1, expand=TRUE, fill="y")

  visible(w) <- TRUE
  
}
}
\seealso{
  \code{\link{gframe}} and \code{\link{gexpandgroup}}
}


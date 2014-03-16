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

  \method{svalue}{GGroup} (obj, index=TRUE, ...) <- value

  gvbox(spacing = 5, use.scrollwindow = FALSE,
    container = NULL, ..., toolkit = guiToolkit())
}
\arguments{
  \item{horizontal}{logical. If TRUE, left to right layout,
  otherwise top to bottom}

  \item{spacing}{spacing aroud widget}

  \item{use.scrollwindow}{logical. Either \code{TRUE},
  \code{"TRUE"}, \code{FALSE}, \code{"FALSE"}, \code{"y"},
  or \code{"x"}. For all toolkits a non-FALSE value will
  place the child components into a scrollable container.
  For some toolkits this will only be in the direction of
  packing. If the toolkit allows it (RGtk2), then values of
  \code{"x"} or \code{"y"} can be used to override the
  default scrolling directions. A box container with
  scrollwindows should have it size set either directly or
  through packing with \code{expand=TRUE} as its size
  request will not reflect the size of its child
  components.}

  \item{obj}{\code{GGroup} object}

  \item{index}{ignored}

  \item{value}{value (in pixels) for between child spacing}

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
\details{
  Child components are typically added to a box container
  through the child components constructor. The argument
  \code{expand}, \code{fill}, and \code{anchor} determine
  how the child is positioned within the container.
}
\examples{
if(interactive()) {
  w <- gwindow("Box containers")
  g <- gvbox(cont=w)                         # ggroup(horizonta=FALSE, ...)
  nb <- gnotebook(cont=g); gbutton("one", label="one", cont=nb)
  gframe("Frame", cont=g)
  pg <- gpanedgroup(cont=g); gbutton("one", cont=pg); gbutton("two", cont=pg)
}
}
\seealso{
  \code{\link{gframe}} and \code{\link{gexpandgroup}}
}


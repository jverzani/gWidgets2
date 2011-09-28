##' @include ggroup.R
NULL

##' Constructor for framed box container with label
##'
##' @param text frame label
##' @param markup does label use markup (toolkit specific)
##' @param pos position of label: 0=left, 1=right, some toolkit allow values in between
##' @param horizontal logical. Direction of layout
##' @param container parent container
##' @param ... passed to add method of parent container
##' @param toolkit toolkit
##' @export
##' @rdname gframe
gframe <- function(
                   text = "", markup=FALSE, pos = 0, horizontal=TRUE, container = NULL,
                   ... ,
                   toolkit=guiToolkit()){
  obj <- .gframe (toolkit,
           text,  markup, pos, horizontal, container,
           ...
           )

  check_return_class(obj, "GFrame")
  obj   
  
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gframe
.gframe <- function(toolkit,
                    text = "", markup = FALSE, pos = 0, horizontal=TRUE,
                    container = NULL,      ... )
           UseMethod( '.gframe' )

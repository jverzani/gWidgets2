##' @include methods.R
NULL

##' Constructor for grid layout container
##'
##' @export
glayout <- function(
                    homogeneous = FALSE, spacing = 10, container = NULL,      ... ,
                    toolkit=guiToolkit()){
  obj <- .glayout (toolkit,
                   homogeneous=homogeneous, spacing=spacing, container=container ,...
                   )


  check_return_class(obj, "GLayout")
  obj   
    
}

##' generic for toolkit dispatch
##'
##' @export
##' @rdname glayout
.glayout <- function(toolkit,
                     homogeneous = FALSE, spacing = 10, container = NULL,
                     ... )
           UseMethod( '.glayout' )

##' pass back item, list or matrix of items depending on dimension
##'
##' @export
##' @rdname glayout
"[.GLayout" <- function(x, i, j, ..., drop=TRUE) {
  getWithDefault(drop, TRUE)

  if(missing(i)) i <- seq_len(dim(x)[1])
  if(missing(j)) j <- seq_len(dim(x)[2])
  
  if(length(i) == 1 && length(j) == 1)
    return(x$get_items(i, j, ..., drop=drop))

  ## a matrix or list
  out <- sapply(j, function(col) lapply(i, function(row) x[row, col]))
  if(is.matrix(out))
    return(out[,,drop=drop])
  else
    return(out)
}



##' add child components to layout using matrix notation
##'
##' The matrix notation allows for spanning of multiple rows and or columns, but no holes.
##' @param x glayout object
##' @param i row index for adding
##' @param j column index for adding
##' @param ... used to pass in values for expand, fill, anchor (see
##' the \code{add} method of \code{ggroup}) for their meaning).
##' @param value a component to add.
##' @export
##' @rdname glayout
"[<-.GLayout" <- function(x, i, j, ..., value) {

  theArgs <- list(...)

  ## get expand, anchor, fill
  expand <- getWithDefault(theArgs$expand, FALSE)
  if(!is.null(theArgs$align))
    theArgs$anchor <- theArgs$align
  anchor <- getWithDefault(theArgs$anchor, NULL)
  fill <- getWithDefault(theArgs$fill, "x") # "", x, y or both

  x$set_items(value, i, j, expand, fill, anchor)
  x
}

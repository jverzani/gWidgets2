##' @include methods.R
NULL

##'  Spinbutton constructor
##'
##' @export
##' @rdname gspinbutton
gspinbutton =function(
  from = 0, to = 10, by = 1,
  length.out = NULL, along.with=NULL,
  value = from, digits = 0,
  handler = NULL, action = NULL, container = NULL, ... ,
  toolkit=guiToolkit()){

  ## mostly from seq.default
  if (!missing(along.with)) {
    length.out <- length(along.with)
  } else if(!missing(length.out)) {
    len <- length(length.out)
    if (!len) 
      stop("argument 'length.out' must be of length 1")
    if (len > 1L) {
      warning("first element used of 'length.out' argument")
      length.out <- length.out[1L]
    }
    length.out <- ceiling(length.out)
  }
  if(!is.null(length.out)) {
    ## set up by to be for length,out
    by <- (to - from)/(length.out[1] - 1)
  }

  obj <- .gspinbutton(toolkit, from, to, by, value, digits,
                      handler, action, container=container, ...)
  check_return_class(obj, "GSpinButton")
  obj
  
}

##' generic for toolkit dispatch
##'
##' @export
##' @rdname gspinbutton
.gspinbutton <- function(toolkit,
                         from = 0, to = 10, by = 1, value = from, digits = 0,
                         handler = NULL, action = NULL, container = NULL, ... ) UseMethod( '.gspinbutton' )

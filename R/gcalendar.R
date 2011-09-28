##' @include methods.R
NULL



##' A constructor for a date selection widget
##'
##' The date is the main property of this widget
##' @param text initial text
##' @param format Date format
##' @param handler handler called when changed
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of parent
##' @param toolkit toolkit
##' @return Returns an object of class \code{GCalendar} for which the following methods are overridden:
##' \enumerate{
##' \item \code{svalue} get the date
##' 
##' \item \code{svalue<-} set the date
##' }
##' The change handler is inherited from \code{\link{gedit}}
##' @export
gcalendar <- function(
                      text = "", format = "%Y-%m-%d", 
                      handler = NULL, action=NULL, container = NULL,...,
                      toolkit=guiToolkit()){
  obj <- .gcalendar (toolkit,
              text=text, format=format, handler=handler,action=action,
              container=container , ...
              )

  check_return_class(obj, "GCalendar")

  obj

}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gcalendar
.gcalendar <- function(toolkit,
                       text = "", format = "%Y-%m-%d", 
                       handler=NULL, action=NULL, container = NULL,
                       ... )
  UseMethod( '.gcalendar' )


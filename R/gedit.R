##' @include methods.R
NULL


##' Single line text edit constructor
##'
##' @param text initial text
##' @param width number of characters
##' @param coerce.with A function or name of function to coerce value with before returning by \code{svalue}
##' @param initial.msg If not text give, and an initial message given, then this message is displayed until the widget receives the focus
##' @param handler Change handler. Called when return key is hit
##' @param action passed to handler
##' @param container parent container
##' @param ... passed to \code{add} method of parent
##' @param toolkit toolkit
##' @return An object of class \code{GEdit}. This has sub-classed methods:
##'
##' \enumerate{
##'
##' \item \code{}
##'
##' \item \code{svalue} to retrieve the text
##'
##' \item \code{svalue<-} to set the text
##'
##' \item \code{[} to get the autocomplete values
##'
##' \item \code{[<-} Character. To set autocomplete values
##'
##' \item \code{visible<-} to specify a character to display instead of text (for passwords)
##'
##' }
##'
##' The default handler call is when the user activates the entry,
##' typically by pressing the enter key.
##'
##' The \code{addhandlerBlur}
##' method is called when the widget loses focuses.
##'
##' The \code{addHandlerKeystroke} method adds a handler called after
##' each keystroke. If possible, the first argument has a component
##' \code{key} passing back the last value.
##' @export
gedit <- function(
                  text = "", width = 25, coerce.with = NULL, initial.msg="",
                  handler = NULL, action = NULL, container = NULL, ... ,
                  toolkit=guiToolkit()) {

  obj <- .gedit(toolkit,
                text=text, width=width, coerce.with=coerce.with, initial.msg=initial.msg,
                handler=handler, action=action, container=container ,...
                )

  check_return_class(obj, "GEdit")
  obj   
}


##' generic for toolkit dispatch
##' 
##' @rdname gedit
##' @export
.gedit <-  function(toolkit,
                    text = "", width = 25, coerce.with = NULL, initial.msg="",
                    handler = NULL, action = NULL, container = NULL, ... ) {
  UseMethod( '.gedit' )
}

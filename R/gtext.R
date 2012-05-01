##' @include methods.R
NULL

##' Multiline text edit constructor
##'
##' The multiline text widget has its main property the text contained
##' within.
##' \itemize{
##' \item{The \code{svalue} will return a string (length-1 character
##' vector) with embedded newlines}
##' \item{The "change" handler is \code{addHandlerKeystroke}}
##' \item{Use \code{addHandlerSelectionChanged} to monitor the selection}
##' }
##' @param text initial text
##' @param width width of widget
##' @param height height of widget (when width is specified)
##' @param font.attr font attributes for text buffer. One can also
##' specify font attributes for insertion. The font attributes are
##' specified with a named vector or list, with names and values
##' coming from:
##' \describe{
##' \item{weight}{ in c("light", "normal", "bold", "heavy")}
##' \item{style}{inc("normal", "oblique", "italic")}
##' \item{family}{in c("sans", "helvetica", "times", "monospace")}
##' \item{size}{in c("xx-small", "x-small", "small", "medium", "large", "x-large", "xx-large")}
##' \item{foreground}{a value in colors()}
##' \item{background}{a value in colors()}
##' }
##' @param wrap logical do lines wrap
##' @inheritParams gwidget
##' @export
##' @rdname gtext
gtext <- function(
                  text = NULL, width = NULL, height = 300,
                  font.attr = NULL, wrap = TRUE,
                  handler = NULL, action = NULL, container = NULL,      ... ,
                  toolkit=guiToolkit()){

  obj <- .gtext(toolkit,
                text=text, width=width, height=height, font.attr=font.attr, wrap=wrap,
                handler=handler, action=action, container=container ,...
                )
  check_return_class(obj, "GText")
  obj
  
}
 

##' generic for toolkit dispatch
##'
##' @export
##' @rdname gtext
.gtext <-  function(toolkit,
                    text = NULL, width = NULL, height = 300, font.attr = NULL,
                    wrap = TRUE,
                    handler = NULL, action = NULL, container = NULL,... )
           UseMethod( '.gtext' )


################### methods ###############################


##' insert text into a gtext buffer
##'
##' @param obj  object
##' @param value text to insert
##' @param where position of insertion
##' @param do.newline logical add a newline at end
##' @return called for side effect
##' @export
##' @rdname gtext
insert <- function(obj,value,
                   where = c("end","beginning","at.cursor"),
                   font.attr=NULL,
                   do.newline=TRUE,
                   ...) UseMethod("insert")

##' insert method for gtext
##'
##' @export
##' @rdname gtext
##' @method insert GText
##' @S3method insert GText
insert.GText <- function(obj, value,
                           where = c("end","beginning","at.cursor"),
                           font.attr=NULL,
                           do.newline=TRUE,
                           ...) {
  obj$insert_text(value,
                  where=match.arg(where),
                  font.attr=sapply(font.attr, identity, simplify=FALSE), # a named list now
                  do.newline, ...)
                           
}

##' dispose method for gtext clears buffer
##'
##' @export
##' @rdname gtext
##' @method dispose GText
##' @S3method dispose GText
dispose.GText <- function(obj, ...) {
  if(isExtant(obj))
    obj$set_value("")
}


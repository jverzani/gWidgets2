##' @include methods.R
##' @include BasicInterface.R
NULL

## Put here until we can figure out how to get past R CMD check

##' A widget for filtering a data frame
##'
##' This widget provides a simple means to subset, or filter, a data
##' frame. 
##' @param DF a data frame or \code{GDf} instance to look variables up within.
##' @param allow.edit logical. If \code{TRUE} a user may add new
##' variables to filter by. If FALSE, then one should specify the
##' variables a user can filter by to \code{initial.vars}.
##' @param initial.vars When given, this is a data frame whose first
##' column specifies the variables within \code{DF} to filter by and
##' whose second column indicates the type of filter desired. The
##' available types are \code{single" to select one from many, 
##' \code{multiple}, for multiple selection; and \code{range}, to
##' specify a from and to value.
##' @inheritParams gwidget
##' @return returns \code{GFilter} object
##' @export
##' @examples
##' \dontrun{
##' DF <- mtcars[, c("mpg", "cyl", "hp", "am", "wt")]
##' w <- gwindow("Example of gfilter", visible=FALSE)
##' pg <- ggroup(container=w)
##' df <- gtable(DF, container=pg)
##' a <- gfilter(df, initial.vars=data.frame(names(DF),
##'                    c("single", "multiple", "range", "single", "range").
##'                    stringsAsFactors=FALSE),
##'              allow.edit=TRUE,
##'              container=pg,
##'              handler=function(h,...) {
##'                visible(df) <- h$obj$get_value()
##'              }
##'              )
##' size(w) <- c(600, 400)
##' visible(w) <- TRUE
##' }
gfilter <- function(DF,
                    allow.edit=TRUE, initial.vars=NULL,
                    handler=NULL, action=NULL,
                    container=NULL,
                    ...,
                    toolkit=guiToolkit()){
  
  
  obj <- .gfilter (toolkit,
                   DF=DF,
                   allow.edit=allow.edit, initial.vars=initial.vars,
                   handler=handler, action=action,
                   container=container,
                   ...
                   )
  check_return_class(obj, "GFilter")
  obj
}




##' generic for toolkit dispatch
##'
##' @export
##' @rdname gfilter
.gfilter <-  function(toolkit,
                      DF,
                      allow.edit=TRUE, initial.vars=NULL,
                      handler=NULL, action=NULL,
                      container=NULL,
                    ... )
           UseMethod( '.gfilter' )


##' svalue method
##'
##' The \code{svalue} method for a filter object returns a logical
##' containing which rows are selected. There is no assignment method.
##' @inheritParams svalue
##' @export
##' @rdname gfilter
##' @method svalue GFilter
##' @S3method svalue GFilter
svalue.GFilter <- function(obj, index=NULL, drop=NULL, ...)   NextMethod()


##' The extraction method returns the child items of the filter, like a container object.
##'
##' @param x the \code{GFilter} object
##' @param i passed to \code{get_items}
##' @param j passed to \code{get_items}
##' @param ... dots argument
##' @export
##' @rdname gfilter
##' @method [ GFilter
##' @S3method [ GFilter
"[.GFilter" <- function(x, i, j, ..., drop=TRUE) {
  if(isExtant(x)) {
    val <- x$get_items(i, j, ..., drop=drop)
    if(!is.null(drop) && drop && length(val) == 1)
      val <- val[[1]]
    return(val)
  } else {
    return(NULL)
  }
}



## Default implementation

##' Default
##'
##' @export
##' @rdname gfilter
##' @method .gfilter default
##' @S3method .gfilter default
.gfilter.default <-  function(
                              toolkit=guiToolkit(),
                              DF,
                              allow.edit=TRUE, initial.vars=NULL,
                              handler=NULL, action=NULL,
                              container=NULL,
                              ... ) {

  obj <- GFilter$new(toolkit,  DF=DF,
                     allow_edit=allow.edit, initial_vars=initial.vars,
                     handler=handler, action=action,
                     container=container,...)

  return(obj)
}



GFilter <- setRefClass("GFilter",
                       contains="GDefaultWidget",
                       fields=list(
                         DF="ANY", 
                         initial_vars="ANY",
                         allow_edit="logical",
                         container="ANY",
                         l="list",
                         types="ANY",
                         handler="ANY",
                         action="ANY"
                         ),
                       methods=list(
                         initialize=function(
                           toolkit, 
                           DF=NULL,
                           initial_vars=NULL,
                           allow_edit=is.null(initial_vars),
                           handler=NULL, action=NULL,
                           container=NULL,
                           ...) {
                           initFields(DF=DF,
                                      initial_vars=initial_vars,
                                      allow_edit=allow_edit,
                                      types  = c("single"="Select one level", "multiple" = "Select multiple levels", "range"="Select range"),
                                      l=list(),
                                      handler=handler,
                                      action=action,
                                      toolkit=toolkit
                                      )
                           connect_df()                              
                           init_ui(container, ...)
                           callSuper()
                         },
                         add_handler_changed=function(handler, action=NULL, ...) {
                           ## we use a different handler mechanism
                           ## here. Each items calls the invoke
                           ## change handler withc in turn uses the
                           ## handler and action properties
                           handler <<- handler
                           action <<- action
                         },
                         invoke_change_handler=function() {
                           "Some value was changed"
                           if(is.null(handler))
                             return()
                           
                           h <- list(obj=.self, action=action)
                           handler(h)
                         },
                         init_ui=function(container, ..., use.scrollwindow) {
                           block <<- ggroup(container=container, horizontal=FALSE, ..., use.scrollwindow=FALSE)
                           container <<- ggroup(container=block, expand=FALSE, horizontal=FALSE)
                           
                           if(allow_edit && !is.null(DF)) {
                             bg <- ggroup(container=block)
                             addSpring(bg)                                
                             btn_add.item <- gbutton(gettext("Add item"), container=bg, handler=function(h,...) {
                               w <- gbasicdialog(gettext("Select a variable and selector type"),
                                                 handler=function(h,...) {
                                                   var <- svalue(varname)
                                                   type <- svalue(type, index=TRUE)
                                                   names(types)[type]
                                                   add_item(var, var, type=type)
                                                 }, parent=h$obj)
                               lyt <- glayout(container=w)
                               lyt[1,1] <- gettext("Variable:")
                               lyt[1,2] <- (varname <- gcombobox(names(DF), selected=0,
                                                                 container=lyt, handler=function(h,...) {
                                 nm <- svalue(h$obj)
                                 var <- DF[[nm]]
                                 if(is.numeric(var)) {

                                   type[] <- types
                                   svalue(type, index=TRUE) <- 1
                                 } else if(is.factor(var) || is.character(var)) {
                                   type[] <- types[1:2]                                   
                                   svalue(type, index=TRUE) <- 2
                                 } else if(is.logical(var)) {
                                   type[] <- types[1:2]
                                   svalue(type, index=TRUE) <- 1
                                 } else {
                                   svalue(type, index=TRUE) <- 2
                                 }
                                 enabled(type) <- TRUE
                               }))
                               lyt[2,1] <- gettext("")

                               
                               lyt[2,2] <- (type <- gradio(types, selected=2, container=lyt))
                               enabled(type) <- FALSE # not until a selecctin is ade
                               visible(w) <- TRUE
                             })
                             btn_add.item$set_icon("add")
                           }
                           addSpring(block) ## push to top
                           
                           ## add initial
                           if(!is.null(initial_vars))
                             sapply(seq_len(nrow(initial_vars)), function(i) {
                               add_item(initial_vars[i,1], name=initial_vars[i,1], type=initial_vars[i,2])
                             })
                           invoke_change_handler()
                         },
                         connect_df=function() {
                           "connect DF to filter"
                           if(is(DF, "GDf"))
                             addHandlerChanged(DF, function(h,...) .self$invoke_change_handler())
                         }, 
                         get_x=function(x) {
                           if(is.character(x) && length(x) == 1)
                             x <- DF[,x]
                           x
                         },
                         add_item=function(x, name=deparse(substitute(x)), type=c("single", "multiple", "range")) {
                           if(missing(type)) 
                             if(is.numeric(get_x(x)))
                               type <- "range"
                             else
                               type <- "multiple"
                           tmp <- type
                           if(is.numeric(type))
                             type <- c("single", "multiple", "range")[type]
                           else
                             type <- match.arg(type)

                           ## dispatch on type
                           if(type == "single")
                             item <- RadioItem$new(x, name=name, parent=.self)
                           else if(type == "multiple")
                             item <- ChoiceItem$new(x, name=name, parent=.self)
                           else
                             item <- RangeItem$new(x, name=name, parent=.self)

                           l <<- c(l, item)
                           item$make_ui(visible=TRUE)
                           invoke_change_handler()
                         },
                         remove_item=function(child) {
                           ## remove from GUI
                           delete(container, child$frame)
                           ## remove from list
                           ind <- gWidgets2:::get_index_in_list(l, child)
                           l[[ind]] <<- NULL
                           invoke_change_handler()
                         },
                         show=function(...) cat("Object for showing filter list\n"),
                         ##
                         ## gWidgets interface: svalue and [
                         ##
                         get_value = function(drop=NULL, ...) {
                           "Return logical of length nrow(df)"
                           if(length(l)) {
                             m <- sapply(l, function(i) i$get_value())
                             apply(m, 1, all)
                           } else {
                             rep(TRUE, nrow(DF))
                           }
                         },
                         get_items=function(i,j, ..., drop=NULL) {
                           "Allow [ to access items"
                           l[i]
                         },
                         get_visible=function() visible(block),
                         set_visible=function(value) visible(block) <<- value,
                         get_enabled=function() enabled(block),
                         set_enabled=function(value) enabled(block) <<- value,
                         set_size=function(value) block$set_size(value)
                         ))



## Filter items
BasicFilterItem <- setRefClass("BasicFilterItem",
                               contains="GDefaultWidget",
                               fields=list(
                                 x="ANY",
                                 name="character",
                                 parent="ANY",
                                 frame="ANY",
                                 widget="ANY"
                                 ),
                               method=list(
                                 initialize=function(x="", name=x, parent=NULL, ...) {
                                   initFields(x=x,
                                              name=name,
                                              parent=parent)
                                   callSuper(...)
                                 },
                                 show=function(...) cat("A filter item\n"),
                                 get_x=function() {
                                   "Get value of x from looking for a data frame"
                                   if(is.character(x) && length(x) == 1) {
                                     DF <- parent$DF
                                     DF[,x]
                                   } else {
                                     x
                                   }
                                 },
                                 make_ui=function(visible=TRUE) {
                                   parent_container <- parent$container # or something else here
##                                   frame <<- gexpandgroup(name, container=parent_container, horizontal=FALSE,
##                                                          expand=TRUE, anchor=c(-1,1))
                                   ##frame$set_visible(visible)
                                   frame <<- gframe(name,  horizontal=FALSE,
                                                    container=parent_container
                                                    )
                                                    ##,
                                                    ##expand=TRUE, fill="x",
                                                    ##anchor=c(-1,1))

                                   make_item_type(container=frame)
                                   f <- function(i) addHandlerChanged(i, handler=function(h,...) {
                                     .self$invoke_change_handler()
                                   })
                                   if(is.list(widget))
                                     sapply(widget, f)
                                   else
                                     f(widget)

                                   g <- ggroup(container=frame, horizontal=TRUE)
                                   addSpring(g)
                                   gbutton("Reset", container=g, handler=function(h,...) {
                                     initialize_item()
                                     .self$invoke_change_handler()
                                   })
                                   if(parent$allow_edit) {
                                     gbutton("Remove", container=g, handler=function(h,...) {
                                       parent$remove_item(.self)
                                     })
                                   }
                                 },
                                 ## need to subclass these
                                 make_item_type=function(container) {
                                   "Make selector for item"
                                 },
                                 initialize_item=function() {
                                   "Method to initialize the item values"
                                 },
                                 invoke_change_handler=function() {
                                   ## pass along
                                   parent$invoke_change_handler()
                                 },
                                 ## gWidgets methods for later use.
                                 get_value=function(...) {
                                   "Return logical of length x"
                                 },
                                 ## pass off to frame
                                 get_visible=function(...) visible(frame),
                                 set_visible=function(value, ...) visible(frame) <<- value,
                                 get_enabled=function(...) enabled(frame),
                                 set_enabled=function(value, ...) enabled(frame) <<- value
                                 ))

RadioItem <- setRefClass("RadioItem",
                         contains="BasicFilterItem",
                         methods=list(
                           make_item_type=function(container) {
                             "Select one from many"
                             u_x <- sort(unique(get_x()))
                             #if(length(u_x) > 4) 
                               widget <<- gcombobox(u_x, container=container, anchor=c(-1,0))
                             #else
                               #widget <<- gradio(u_x, container=container, horizontal=TRUE, anchor=c(-1,0))

                             if(is.numeric(u_x))
                               widget$coerce_with <<- as.numeric
                             
                             initialize_item()
                           },
                           initialize_item = function() {
                             svalue(widget, index=TRUE) <<- 1L
                           },
                           get_value=function(...) {
                             get_x() == svalue(widget)
                           }
                           ))

ChoiceItem <- setRefClass("ChoiceItem",
                          contains="BasicFilterItem",
                           methods=list(
                           make_item_type=function(container) {
                             "Select one from many"
                             u_x <- sort(unique(get_x()))
                             use.table <- length(u_x) > 4
                             widget <<- gcheckboxgroup(u_x, container=container,
                                                       use.table=use.table,
                                                       expand=TRUE, fill=TRUE
                                                       )
                             if(length(u_x) >= 4){
                                size(widget) <<- list(height= 4 * 25)
                             } else {
                                size(widget) <<- list(height= length(u_x) * 25)
                             }
                             if(is.numeric(u_x))
                               widget$coerce_with <<- as.numeric
                             
                             initialize_item()
                           },
                           initialize_item = function() {
                             svalue(widget, index=TRUE) <<- TRUE # all selected
                           },
                           get_value=function(...) {
                             get_x() %in% svalue(widget)
                           }
                           ))

RangeItem <- setRefClass("RangeItem", 
                         contains="BasicFilterItem",
                         methods=list(
                           make_item_type=function(container) {
                             "a <= widget <= b"
                             widget <<- list()

                             g <- ggroup(container=container, expand=TRUE, fill="y")
                             g1 <- ggroup(container=g, horizontal=FALSE, expand=TRUE)
                             widget[[1]] <<- gedit("", container=g1, width=10, coerce.with=as.numeric)

                             glabel(gettext("to"), container=g)
                             
                             g2 <- ggroup(container=g, horizontal=FALSE, expand=TRUE)
                             widget[[2]] <<- gedit("", container=g2, width=10, coerce.with=as.numeric)
                             initialize_item()
                             
                           },
                           initialize_item=function() {
                             sapply(widget, function(i) {
                               svalue(i) <- ""
                               i[] <- sort(unique(get_x()))
                             })
                             widget[[1]]$set_value(min(get_x()))
                             widget[[2]]$set_value(max(get_x()))

                           },
                           get_value=function(...) {
                             a <- svalue(widget[[1]])
                             if(is.null(a) || is.na(a))
                               a <- -Inf
                             b <- svalue(widget[[2]])
                             if(is.null(b) || is.na(b))
                                b <- Inf
                             a <= get_x() & get_x() <= b
                           }
                           ))
                                




  

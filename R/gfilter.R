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
                         na_filter = "ANY",
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

                               
                               lyt[2,1] <- gettext("Edit by")

                               
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
                               add_item(initial_vars[i,1], name=initial_vars[i,1], type=initial_vars[i,2], TRUE)
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
                                 includeNA="ANY",
                                 parent="ANY",
                                 frame="ANY",
                                 widget="ANY"
                                 ),
                               method=list(
                                 initialize=function(x="", name=x, parent=NULL, includeNA=TRUE,...) {
                                   initFields(x=x,
                                              name=name,
                                              parent=parent,
                                              includeNA=includeNA
                                              )
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

                                   make_buttons(frame)
                                 },
                                 ## need to subclass these
                                 make_item_type=function(container) {
                                   "Make editor for item"
                                 },
                                 make_buttons=function(frame) {
                                   g <- ggroup(container=frame, horizontal=TRUE)

                                   includeNA <<- gcheckbox("Include NA", checked=FALSE, cont=g)
                                   addHandlerChanged(includeNA, function(...) {
                                     parent$invoke_change_handler()
                                   })
                                   
                                   addSpring(g) # right justify
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
                                 do_na = function() svalue(includeNA),
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
                             val <- svalue(widget)
                             if(length(val) == 0) # might have no choices in widget (all NA), This helps..
                               val <- NA

                             out <- get_x() == val
                             out[is.na(out)] <- do_na()
                             out
                           }
                           ))

ChoiceItem <- setRefClass("ChoiceItem",
                          contains="BasicFilterItem",
                          fields=list(
                            "old_selection"="ANY",
                            "search_type"="ANY"
                            ),
                           methods=list(
                           make_item_type=function(container) {
                             "Select one from many"
                             u_x <- as.character(sort(unique(get_x(), na.rm=TRUE)))
                             use.table <- length(u_x) > 4 # XXX make 4 part of parent so it can be configure
                             vb <- gvbox(container=container)
                             search_type <<-  1 # Regular expression is default
                             if(use.table) {
                               gp <- ggroup(cont=vb)
                               ed <- gedit("", initial.msg="Filter values by...", expand=TRUE, container=gp)
                               ed$set_icon("ed-search", "start")
                               ed$set_icon("ed-remove", "end")
                               ed$set_icon_handler(function(h,...) {
                                 svalue(ed) <- ""
                               }, where="end")
                               
                               b <- gbutton("opts", cont=gp)
                               r <- gradio(c("Regular expression",
                                             "Fixed",
                                             "Ignore case"), 
                                           handler=function(h,...) {
                                             search_type <<- svalue(r, index=TRUE)
                                           })
                               svalue(r, index=TRUE) <- search_type
                               addPopupMenu(b, gmenu(list(r), popup=TRUE))
                               
                               handler=function(h,...) {
                                 ## we keep track of old selection here
                                 ## that updates only when user changes selection, not when filter does
                                 cur_sel <- old_selection
                                 blockHandlers(widget)
                                 on.exit(unblockHandlers(widget))
                                 val <- svalue(h$obj)

                                 ## how to filter
                                 f <- function(u) grepl(val, u)
                                 if(search_type == 2)
                                   ## fixed, 
                                   f <- function(u) grepl(val, u, fixed=TRUE)
                                 else if(search_type == 3)
                                   f <- function(u) grepl(val, u, ignore.case=TRUE)
                                 
                                 if(val == "")
                                   widget[] <<- u_x
                                 else
                                   widget[] <<- Filter(f, u_x)
                                 svalue(widget) <<- cur_sel
                                 old_selection <<- cur_sel
                               }
                               addHandlerKeystroke(ed, handler)
                               addHandlerChanged(ed, handler)
                             }
                             
                             widget <<- gcheckboxgroup(u_x, container=container,
                                                       use.table=use.table,
                                                       expand=TRUE, fill=TRUE
                                                       )
                             addHandlerChanged(widget, function(h,...) {
                               old_selection <<- svalue(h$obj)
                             })
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
                           make_buttons=function(frame) {
                             g <- ggroup(container=frame, horizontal=TRUE)

                             includeNA <<- gcheckbox("Include NA", checked=FALSE, cont=g)
                             addHandlerChanged(includeNA, function(...) {
                               parent$invoke_change_handler()
                             })
                             
                             addSpring(g) # right justify
                             gbutton("Reset", container=g, handler=function(h,...) {
                               initialize_item()
                               .self$invoke_change_handler()
                             })
                             gbutton("Clear", container=g, handler=function(h,...) {
                               ## LIVIU to fill in with better?
                               svalue(widget) <<- FALSE
                               .self$invoke_change_handler()
                             })
                             if(parent$allow_edit) {
                               gbutton("Remove", container=g, handler=function(h,...) {
                                 parent$remove_item(.self)
                               })
                             }
                           },
                           get_value=function(...) {
                             out <- get_x() %in% svalue(widget)
                             na_vals <- is.na(get_x())
                             out[na_vals] <- do_na()
                             out
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
                             widget[[1]] <<- gedit("", container=g1, width=10)

                             glabel(gettext("to"), container=g)
                             
                             g2 <- ggroup(container=g, horizontal=FALSE, expand=TRUE)
                             widget[[2]] <<- gedit("", container=g2, width=10)
                             initialize_item()
                             
                           },
                           initialize_item=function() {
                             sapply(widget, function(i) {
                               svalue(i) <- ""
                               i[] <- sort(unique(get_x(), na.rm=TRUE))
                             })
                             widget[[1]]$set_value(min(get_x(), na.rm=TRUE))
                             widget[[2]]$set_value(max(get_x(), na.rm=TRUE))

                           },
                           get_value=function(...) {
                             vals <- get_x()
                             a <- svalue(widget[[1]])
                             b <- svalue(widget[[2]])
                             asx <- function(x, a) UseMethod("asx")
                             asx.default <- function(x, a) as.numeric(a)
                             asx.integer <- function(x, a) as.integer(a)
                             asx.character <- function(x, a) as.character(a)
                             asx.POSIXct <- function(x, a) as.POSIXct(a)
                             asx.Date <- function(x, a) as.Date(a)
                             a <- asx(vals,a); b <- asx(vals, b)
                             no_a <- is.null(a) || is.na(a)
                             no_b <- is.null(b) || is.na(b)


                             if(no_a & no_b) {
                               out <- rep(TRUE, length(vals))
                             } else if(no_a) {
                               out <- (vals <= b)
                             } else if (no_b) {
                               out <- (a <= vals)
                             } else {
                               out <- (a <= vals & vals <= b)
                             }
                             out[is.na(out)] <- do_na()
                             out
                           }
                           ))
                                




  

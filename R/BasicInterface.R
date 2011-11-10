##' @include guiToolkit.R
##' @include misc.R
NULL

## simple message function
define_me <- function(...) {
  curcall <- as.character(sys.call()[[1]])[3]
  message(sprintf("Method %s not defined for class %s\n",
                  curcall,
                  class(get(".self"))          # issue with warning
                  ))
}

Observer <- setRefClass("Observer",
                        fields=list(
                          o = "ANY",   
                          obj="ANY"
                          ),
                        methods=list(
                          initialize=function(o=NULL, obj=NULL) {
                            initFields(o=o, obj=obj)
                            callSuper()
                          },
                          update=function(...) {
                            "Call self."
                            o(obj, ...)
                          }
                          )
                        )

## Handler is a special observer with obj and actino passed as first argument
Handler <- setRefClass("Handler",
                       contains="Observer",
                       fields=list(
                         action="ANY"
                         ),
                        methods=list(
                          initialize=function(o=NULL, obj=NULL, action=NULL) {
                            initFields(action=action)
                            callSuper(o=o, obj=obj)
                          },
                          update=function(...) {
                            "Call self."
                            o(list(obj=obj, action=action), ...)
                          }
                          )
                        )

##' constructor for handler object
##'
##' not exported, call using :::
##' 
observer <- function(receiver, handler, action=NULL) 
  Handler$new(handler, receiver, action)


##' Observable class sets up objects that can be observed. Inherited by template
##'
##' @exportClass Observable
Observable <- setRefClass("Observable",
                          fields=list(
                            ..observers="list",
                            ..blocked_observers = "list",
                            ..blocked="integer"
                            ),
                          methods=list(
                            add_observer=function(o, signal="DEFAULT") {
                              "Add an observer. Return id for block/remove/..."
                              if(!is(o, "Observer"))
                                stop("Not an observer")
                              l <- ..observers
                              if(is.null(l[[signal]]))
                                l[[signal]] <- list(o)
                              else
                                l[[signal]] <- c(l[[signal]], o)
                              ..observers <<- l
                              list(signal=signal, o=o)
                            },
                            remove_observer=function(id) {
                              "Remove observer"
                              if(!is(id$o, "Observer"))
                                stop("Call with an observer id")
                              
                              signal <- id$signal
                              ind <- lapply(..observers[[signal]], function(i) identical(i, id$o))
                              if(any(unlist(ind)) )
                                ..observers[[signal]][[which(ind)]] <<- NULL
                              
                            },
                            block_observers=function() {
                              "Block all observers"
                              if(is("..blocked", "uninitializedField") || length(..blocked) == 0) {
                                ..blocked <<- 1L
                              } else {
                                ..blocked <<- ..blocked + 1L
                              }
                            },
                            unblock_observers=function() {
                              "Remove block of all observers. Keeps count, so may need to call again"
                              if(is("..blocked", "uninitializedField") || length(..blocked) == 0) {
                                ..blocked <<- 0L
                              } else {
                                ..blocked <<- max(..blocked - 1L, 0L)
                              }
                              invisible(..blocked)
                            },
                            
                            block_observer=function(id) {
                              "Block observers. If o missing, block all"
                              if(missing(id) || is.null(id)) {
                                block_observers()
                              } else {
                                if(is.null(..blocked_observers[[id$signal]]))
                                  ..blocked_observers[[id$signal]] <<- list(id$o)
                                else
                                  ..blocked_observers[[id$signal]] <<-
                                    c(..blocked_observers[[id$signal]], o)
                              }
                            },
                            unblock_observer=function(id) {
                              "Unblock observer. If id missing, unblock global block"
                              if(missing(id) || is.null(id)) {
                                unblock_observers()
                              } else {
                                signal <- id$signal
                                ind <- lapply(..blocked_observers[[signal]], function(i) identical(i, id$o))
                                if(any(unlist(ind))) 
                                  ..blocked_observers[[signal]][[which(ind)]] <<- NULL
                              }
                            },
                            notify_observers=function(..., signal="DEFAULT") {
                              "Call each non-blocked observer"
                              if(!is("..blocked", "uninitializedField") && length(..blocked) && ..blocked > 0)
                                return()
                              QT <- lapply(..observers[[signal]], function(o) {
                                ind <- lapply(..blocked_observers[[signal]], function(i) identical(i, o))
                                if(!any(unlist(ind))) 
                                  o$update(...)
                              })
                            }
                            )
                          )

##' Basic interface for a widget. These are methods referenced by the S3 methods
##'
##' This interface is inherited by the base GComponent classes in the
##' toolkit implementations. The methods defined here are referenced
##' by the S3 methods. For exampe, \code{svalue} dispatches to
##' \code{get_value}.
##'
##' We combine both widget and container methods here. It isn't
##' perfect, but they do share quite a bit. Perhaps, we could make the
##' container class subclass the basic interface.
##' @exportClass BasicToolkitInterface
BasicToolkitInterface <- setRefClass("BasicToolkitInterface",
                                     contains="Observable",
                                     fields=list(
                                       toolkit="ANY",
                                       widget="ANY",
                                       block="ANY",
                                       parent="ANY", # NULL for gwindow, else parent container
                                       default_expand="LogicalCharacterOrNULL",
                                       default_fill="LogicalCharacterOrNULL"
                                       ),
                                     methods=list(
                                       get_value=define_me, # svalue
                                       set_value=define_me, # svalue<-
                                       get_index=define_me, # svalue; index=TRUE
                                       set_index=define_me,   # svalue <-; index=TRUE
                                       get_enabled=define_me, # enabled
                                       set_enabled=define_me, # enabled<-
                                       get_visible=define_me, # visible
                                       set_visible=define_me, # visible<-
                                       get_editable=define_me, # editable
                                       set_editable=define_me, # editable<-
                                       get_focus=define_me,    # foucs
                                       set_focus=define_me,    # focus<-
                                       get_font=define_me,    # font
                                       set_font=define_me,    # font<-
                                       get_length=define_me,  # length
                                       set_length=define_me,  # length<-
                                       get_dim=define_me,     # dim
                                       get_names=define_me,   # names
                                       set_names=define_me,   # names<-
                                       get_dimnames=define_me, # dimnames
                                       set_dimnames=define_me, # dimnames <-
                                       get_items=define_me,   # [
                                       set_items=define_me,   # [<-
                                       get_attr=define_me,    # tag
                                       set_attr=define_me,    # tag<-
                                       update_widget=define_me, # update
                                       is_extant=define_me,     # isExtant
                                       undo=define_me,          # undo
                                       redo=define_me,          # redo
                                       add_child=define_me,     # add child to container (if present)
                                       add_handler=define_me,
                                       add_handler_changed=define_me,
                                       add_handler_clicked=define_me,
                                       add_handler_double_clicked=define_me,
                                       add_handler_right_clicked=define_me,
                                       add_handler_column_clicked=define_me,
                                       add_handler_column_double_clicked=define_me,
                                       add_handler_column_right_clicked=define_me,
                                       add_handler_select=define_me,
                                       add_handler_focus=define_me,
                                       add_handler_blur=define_me,
                                       add_handler_destroy=define_me,
                                       add_handler_unrealize=define_me,
                                       add_handler_expose=define_me,
                                       add_handler_keystroke=define_me,
                                       add_handler_mouse_motion=define_me,
                                       add_popup_menu=define_me,
                                       add_3rd_mouse_popup_menu=define_me,
                                       add_drop_source=define_me,
                                       add_drop_target=define_me,
                                       add_drag_Motion=define_me,
                                       emit_signal=define_me, # gSignalEmit, ... to invoke. Signal missing do change one
                                       ## show method
                                       show=function() {
                                         cat(sprintf("An object of class %s\n", class(.self)[1]))
                                       }
                                       ))


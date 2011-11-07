##' @include misc.R
NULL

##'  helper function to bypass lack of cached value in method call
##'
##' @param meth method name
##' @param obj method of object's class
##' @return the method
##' @note use as do.call(call_meth, args)
call_meth <- function(meth, obj) {
  if(exists(meth, obj, inherits=FALSE))
    get(meth, obj)
  else
    methods:::envRefInferField(obj, meth, getClass(class(obj)), obj)
}


## Command and CommandStack classes

##' Command class
##'
##' Class for commands. Has methods do, redo, undo
##' @exportClass Command
Command <- setRefClass("Command",
                       fields=list(
                         receiver="ANY",
                         meth="character",
                         params="list",
                         old_params="list"
                         ),
                       methods=list(
                         initialize=function(receiver="", meth="", ...) {
                           l <- list(...)
                           initFields(receiver=receiver, meth=meth, params=l, old_params=l)
                           callSuper()
                         },
                         execute=function(args, meth_name=meth) {
                           do.call(call_meth(meth_name, receiver), args)
                         },
                         do=function() {
                           out <- execute(params)
                           old_params$value <<- out
                         },
                         redo=function() execute(params),
                         undo=function() execute(old_params)
                         ))
                       
                       
## Sample subclass
## cmd <- setRefClass("OtherCommand", contains="Command",
##                    methods=list(
##                      undo=function() message("huh")
##                    ))$new("Fred", "meth_name", "value")
## cmd$undo()

##' Class for multple commands
##'
##' @exportClass CommandList
CommandList <- setRefClass("CommandList",
                           fields=list(
                             l="list"
                             ),
                           methods=list(
                             initialize=function(...) {
                               initFields(l=list(...))
                               callSuper()
                             },
                             add=function(cmd) {
                               l[[length(l) + 1]] <<- cmd
                             },
                             do=function() {
                               sapply(l, function(i) i$do())
                             },
                             redo=function() {
                               sapply(l, function(i) i$redo())
                             },
                             undo=function() {
                               sapply(l, function(i) i$undo())
                             }
                             ))

##' Stack to hold commands
##' 
##' A list with ptr. delegates call of do or undo to appropriate command
##' @exportClass CommandStack
CommandStack <- setRefClass("CommandStack",
                            fields=list(
                              l="list",
                              ptr="integer"
                              ),
                            methods=list(
                              initialize=function() {
                                initFields(l=list(), ptr=0L)
                                callSuper()
                              },
                              do=function() {
                                if(!can_do()) return()
                                cmd <- l[[ptr]]
                                ptr <<- ptr + 1L
                                cmd$do()
                              },
                              undo=function() {
                                if(!can_undo()) return()
                                cmd <- l[[ptr-1]]
                                ptr <<- ptr - 1L
                                cmd$undo()
                              },
                              redo=function() {
                                if(!can_do()) return()
                                cmd <- l[[ptr]]
                                ptr <<- ptr + 1L
                                cmd$redo()
                              },
                              can_do=function() {
                                ptr > 0 && ptr <= length(l)
                              },
                              can_undo=function() {
                                ptr > 1
                              },
                              add=function(cmd, call=TRUE) {
                                if(ptr <= 1) {
                                  l <<- list(cmd)
                                  ptr <<- 1L
                                } else {
                                  l <<- l[1:(ptr-1)]
                                  l[[length(l) + 1]] <<- cmd
                                }
                                if(call)
                                  do()
                              },
                              clear=function(cmd) {
                                l <<- list(); ptr <<- 0L
                              }
                              ))
                            
                            
                            

## ## Test commands
## cs <- CommandStack$new()
## cmd1 <- Command$new(.GlobalEnv, "print", x=1)
## cs$add(cmd1)
## cmd2 <- Command$new(.GlobalEnv, "print", x=2)
## cmd3 <- Command$new(.GlobalEnv, "print", x=3)
## cs$add(cmd2)
## cs$undo()
## cs$do()
## cs$undo()
## cs$add(cmd3)


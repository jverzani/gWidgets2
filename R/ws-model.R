##' @include BasicInterface.R
NULL


##' A class for monitoring workspacke
##'
##' A reference class to create a model that monitors the global workspace. The class has
##' methods \code{update_state}, \code{start_timer}, \code{stop_timer} and the "getting" methods
##' \code{get_by_class}, \code{get_by_function} (filter), \code{get_changes}
##' @exportClass WSWatcherModel
##' @rdname S4-classes
##' @name WSWatcherModel-class
WSWatcherModel <-  setRefClass("WSWatcherModel",
                               contains="Observable",
                               fields=list(
                                 timer="ANY",
                                 nms="character",
                                 digests="ANY",
                                 old_nms = "character",
                                 old_digests="ANY",
                                 changes = "list" # should signal when updated
                                 ),
                               methods=list(
                                 initialize=function(toolkit=NULL, ...) {
                                   "Initialze state of cached objects"

                                   timer <<- gtimer(ms=1000, FUN=function(ws_model) {
                                     ws_model$update_state()
                                   }, data=.self, start=FALSE, toolkit=toolkit)
                                   
                                   update_state() # initial
                                   old_nms <<- nms
                                   old_digests <<- digests
                                   callSuper(...)
                                 },
                                 update_timer=function(n) {
                                   "Adjust timer to size of workspace"
                                   if(timer$started) {
                                     cur <- timer$interval
                                     suggested <- floor(log(1 + n)) * 1000 # XXX too long?
                                     if(cur != suggested) {
                                       stop_timer()
                                       start_timer(suggested)
                                     }
                                   }
                                 },
                                 update_state=function(...) {
                                   "update cache of names/digests"
                                   nms <<- ls(envir=.GlobalEnv)
                                   ## avoid certain classes
                                   skip_these <- function(x) !(is(x, "DigestClass") || is(x, "envRefClass"))
                                   digests <<- sapply(Filter(skip_these, mget(nms, .GlobalEnv)), digest)
                                   if(any_changes()) {
                                     ## Update the "changes"
                                     is_changed <- function(i) digests[i] != old_digests[i]
                                     changes <<- list(removed=setdiff(old_nms, nms),
                                                      added=setdiff(nms, old_nms),
                                                      changed=Filter(is_changed, intersect(old_nms, nms))
                                                      )
                                     old_nms <<- nms
                                     old_digests <<- digests
                                     notify_observers()
                                   }
                                   update_timer(length(nms))
                                 },
                                 any_changes=function(...) {
                                   "Report  if any changes"
                                   if(length(old_nms) == 0) {
                                     out <- TRUE
                                   } else  {
                                     out <- (length(old_digests) != length(digests)) || any(old_digests != digests)
                                   }
                                   out
                                 },
                                 start_timer=function(ms) {
                                   "Start timer. Can modify ms here"
                                   if(!missing(ms)) 
                                     timer$set_interval(ms)
                                   timer$start_timer()
                                 },
                                 stop_timer=function() {
                                   timer$stop_timer()
                                 },
                                 ## get
                                 get_by_class = function(classes=character(0)) {
                                   "Return objects matching any of classes"
                                   if(length(classes) == 0)
                                     return(nms)
                                   f <- function(x) Reduce("||", sapply(classes, is, object=x))
                                   get_by_function(f)
                                 },
                                 get_by_function= function(f) {
                                   "Filter objects by function f"
                                   objs <- mget(nms, .GlobalEnv, ifnotfound=list(function(x) {}))
                                   Filter(f, objs)
                                 },
                                 get_changes=function() {
                                   "Return list of changes"
                                   changes
                                 }
                                 ))

##' @include ws-model.R
NULL


##' Constructor for workspace variable browser
##'
##' A workspace browser widget.
##' @inheritParams gwidget
##' @export
##' @rdname gvarbrowser
gvarbrowser <- function(
                        handler = NULL,
                        action = "summary",
                        container = NULL ,...,
                        toolkit=guiToolkit()){
  obj <- .gvarbrowser (toolkit,
                          handler=handler, action=action, container = container, ...
                          )
  check_return_class(obj, "GVarBrowser")
  return(obj)
}


##' generic for toolkit dispatch
##'
##' @inheritParams gvarbrowser
##' @rdname gvarbrowser
.gvarbrowser <-  function(toolkit,
                          handler = NULL,
                          action = "summary", container = NULL,... )
  UseMethod( '.gvarbrowser' )



## Can override via option gWidgets2:gvarbrowser_classes
gvarbrowser_default_classes <- list("Data"=c("integer", "numeric",  "matrix", "character", "logical"),
                                    "Data sets"=c("data.frame", "list"),
                                    "Models"=c("lm", "rlm"),
                                    "Functions"=c("function")
                                    )

##################################################

##' Provide a short summary for an object
##'
##' @param x object
##' @export
##' @rdname short_summary
short_summary <- function(x) UseMethod("short_summary")

##' method for generic
##'
##' @inheritParams short_summary
##' @export
##' @rdname short_summary
short_summary.default <- function(x) sprintf("Object of class %s", class(x)[1])

##' method for generic
##'
##' @inheritParams short_summary
##' @export
##' @rdname short_summary
short_summary.numeric <- function(x) sprintf("Numeric object of length %s", length(x))

##' method for generic
##'
##' @inheritParams short_summary
##' @export
##' @rdname short_summary
short_summary.data.frame <- function(x) sprintf("data frame %s variables %s cases", ncol(x), nrow(x))

##' method for generic
##'
##' @inheritParams short_summary
##' @export
##' @rdname short_summary
short_summary.lm <- function(x) sprintf("Linear model")

##' method for generic
##'
##' @inheritParams short_summary
##' @export
##' @rdname short_summary
short_summary.function <- function(x) "Function"

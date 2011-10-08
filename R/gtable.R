##' @include methods.R
NULL





##' A constructor for displaying tabular data for selection
##'
##' The tabular widget allows a user to select one (or more) row(s)
##' using the mouse or keyboard selection. The selected rows are the
##' main property and are returne by svalue through their key (from
##' the column specified by \code{chosen.col}), or by index. The
##' change handler changes on change of selection. Use add handler
##' click and add handler double click as well.
##' @param items data.frame specifies items for selection. May be a vector, matrix or data frame
##' @param multiple logical allow  multiple selectino
##' @param chosen.col which value from the row is returned by selection
##' @param icon.col NULL or integer. If latter, specifies column containing stock icon
##' @param tooltip.col NULL or integer. If latter, specifies column containing tooltip
##' @param handler
##' @param action 
##' @param container 
##' @param ... 
##' @param toolkit
##' @export
gtable <- function(
                   items,
                   multiple = FALSE,
                   chosen.col = 1,
                   icon.col = NULL,
                   tooltip.col=NULL,
                   handler = NULL, action = NULL,
                   container = NULL, ... ,
                   toolkit=guiToolkit()){


  deprecated_args=list(
    "chosencol"="Use chosen.col",
    "icon.FUN" = "Use icon.col to specify column of items storing stock icon name"
    )
  check_deprecated(deprecated_args)
  
  ## coerce items
  if(!missing(items)) {
    if (is.vector(items))
      items <- data.frame(Values=items, stringsAsFactors=FALSE)
    if(is.matrix(items))
      items <- data.frame(items, stringsAsFactors=FALSE)
  }
  
  obj <- .gtable (toolkit,
                  items=items,
                  multiple=multiple,
                  chosen.col=chosen.col,
                  icon.col = icon.col,
                  tooltip.col = tooltip.col,
                  handler=handler,
                  action=action,
                  container=container ,...
                     )

  obj <- new( 'gTable',widget=widget,toolkit=toolkit) 
  check_return_class(obj, "GTable")
  return(obj)
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gtable
.gtable <- function(toolkit,
                    items,
                    multiple = FALSE,
                    chosen.col = 1,
                    icon.col = NULL,
                    tooltip.col=NULL,
                    handler = NULL, action = NULL,
                    container = NULL,
                    ... )
           UseMethod( '.gtable' )


## example with filter
## add change, doubleclick
## svlaue by chosencol (or index=TRUE)
## visible does filtering

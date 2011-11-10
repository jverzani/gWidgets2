##' @include gdf.R
NULL


##' A notebook container for many \code{gdf} instances
##'
##' @export
##' @param items data frame for initial page, when given
##' @param container parent container
##' @param ... passed to \code{add} method of parent container
##' @param toolkit toolkit
gdfnotebook <- function(
                        items = NULL, container = NULL, ... ,
                        toolkit=guiToolkit()){

  if(is.character(toolkit))
    toolkit <- guiToolkit(toolkit)

  obj <- .gdfnotebook (toolkit,
                       items=items, container=container ,...
                       )
  check_return_class(obj, "GDfNotebook")
  return(obj)
}


##' S3 generic whose methods are implemented in the toolkit packages
##'
##' @rdname gdfnotebook
##' @export
.gdfnotebook <- function(toolkit, items,  container, ...) UseMethod(".gdfnotebook")

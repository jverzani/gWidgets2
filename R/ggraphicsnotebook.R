##' @include ggraphics.R
NULL


ggraphicsnotebook <- function(
                              width = dpi * 6, height = dpi * 6, dpi = 75, container = NULL,      ... ,
                              toolkit=guiToolkit()){

  
  if(is.character(toolkit))
    toolkit <- guiToolkit(toolkit)

  obj <- ggraphicsnotebook (toolkit,
                            width=width, height=height, dpi=dpi, container=container ,...
                            )
  check_return_class(obj, "GGraphicsNotebook")
  return(obj)
}


##' S3 generic whose methods are implemented in the toolkit packages
##'
##' @rdname gdfnotebook
##' @export
.ggraphicsnotebook <- function(toolkit, width, height, dpi, container, ...) UseMethod(".ggraphicsnotebook")


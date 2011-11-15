##' @include ggraphics.R
NULL

##' A notebook widget holding plot devices
##'
##' @param width width in pixels
##' @param height height in pixels
##' @param dpi screen resolution
##' @inheritParams gwidget
##' @export
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
##' @rdname ggraphicsnotebook
##' @export
.ggraphicsnotebook <- function(toolkit, width, height, dpi, container, ...) UseMethod(".ggraphicsnotebook")


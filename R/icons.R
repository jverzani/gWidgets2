##' @include methods.R
NULL

##' Method to add icon to list of stock icons
##'
##' @param iconsNames names of icons 
##' @param iconFiles path of icons
##' @param ... ignored
##' @param toolkit used to dispatch into toolkit if a separate implementation is made
##' @export
##' @rdname icons
addStockIcons <- function(iconNames,iconFiles, ..., toolkit = guiToolkit()) {
  .addStockIcons (toolkit, iconNames, iconFiles, ...)
}

##' generic for dispath
##'
##' @export
##' @rdname icons
.addStockIcons <-  function(toolkit, iconNames, iconFiles,... )
           UseMethod( '.addStockIcons' )


.addStockIcons.default <- function(toolkit, iconNames, iconFiles,... ) {
  ## default implementation
  cur <- .gWidgetsIcons$icons
  if(length(iconNames) == length(iconFiles))
    sapply(seq_len(length(iconNames)), function(i) {
      cur[[iconNames[i]]] <- iconFiles[i]
    })

  .gWidgetsIcons$set_icons(cur)

}

##' return list of available stock icons
##'
##' @return list of icons with names the icon name and values the icon file name or icon object (as needed by the toolkit)
##' @export
##' @rdname icons
getStockIcons = function( ..., toolkit = guiToolkit()) {
  out =  .getStockIcons (toolkit,...)
  return(out)
}

##' generic for toolkit dispatch
##' @rdname icons
.getStockIcons <- function(toolkit,...)
           UseMethod( '.getStockIcons' )


##' default
##'
##' @rdname icons
.getStockIcons.default <- function(toolkit, ...) {
  .gWidgetsIcons$icons
}


##' Return stock icon name, filename, icon object from its by name
##'
##' @export
##' @rdname icons
getStockIconByName <- function(name, ..., toolkit=guiToolkit())
  .getStockIconByName(toolkit, name, ...)

##' generic
##'
##' @rdname icons
.getStockIconByName <- function(toolkit, name, ...) UseMethod(".getStockIconByName")

##' default implementation
##'
##' @param file logical If TRUE, return filename. If FALSE, return toolkit icon object (if possible).
##' @rdname icons
.getStockIconByName.default <- function(toolkit, name, file=TRUE, ...) {
  icons <- .gWidgetsIcons$icons
  i <- match(name, names(icons))
  i <- i[!is.na(i)]
  if(length(i) == 0)
    return(NULL)
  if(length(i) == 1)
    icons[i][[1]]
  else
    icons[i]                            # as a list
}

##' Find a stock icon from the given class
##'
##' @export
##' @rdname icons
stockIconFromClass = function(theClass, ..., toolkit = guiToolkit()) {
  .stockIconFromClass (toolkit, theClass, ...)
}

##' generic for dispath
##'
##' @param theClass name of class
##' @export
##' @rdname icons
'.stockIconFromClass' <- function(toolkit, theClass,... )
           UseMethod( '.stockIconFromClass' )


##' Default stock icon for a given class name
##'
##' @rdname icons
.stockIconsFromClass.default <- function(toolkit, theClass, ...) {

  switch(theClass[1], 
         numeric = "numeric.gif",
         character = "character.gif",
         factor = "factor.gif",
         data.frame = "dataframe.gif",
         matrix = "matrix.gif",
         lm = "model.gif",
         "symbol-dot.gif")
         
}


##' Find stock icon from the given object
##'
##' @return name of icon.
##' @export
##' @rdname icons
stockIconFromObject <- function(obj, ..., toolkit = guiToolkit()) {
  .stockIconFromClass (toolkit, obj, ...)
}

##' generic for dispath
##'
##' @rdname icons
.stockIconFromObject <- function(toolkit, obj,... )
           UseMethod( '.stockIconFromObject' )


.stockIconFromObject.default <- function(toolkit, obj, ...) {
  .icon <- function(x) UseMethod(".icon")
  .icon.default <- function(x) "symbol-dot.gif"
  .icon.numeric <- function(x) "numeric.gif"
  .icon.character <- function(x) "character.gif"
  .icon.factor <- function(x) "factor.gif"
  .icon.data.frame <- function(x) "dataframe.gif"
  .icon.matrix <- function(x) "matrix.gif"
  .icon.lm <- function(x) "model.gif"

  nm <- .icon(obj)
  .gWidgetsIcons$get_icon_from_name(nm)
}

##' Class for icons
##'
##' @exportClass GWidgets2Icons
GWidgets2Icons <- setRefClass("GWidgets2Icons",
                     fields=list(
                       icons="list"
                       ),
                     methods=list(
                       initialize=function() {
                         update_icons()
                         callSuper()
                       },
                       update_icons = function() {
                         path <- system.file("images", package="gWidgets2")
                         allIcons <- Filter(function(i) i != "README", list.files(path))
                         
                         ## create a hash with name -> location
                         l <- list()
                         for(i in allIcons) {
                           filename <- sub("\\.xpm$|\\.gif$|\\.jpg$|\\.jpeg$|\\.png$|\\.tiff$","",i)
                           l[[filename]] <- system.file("images", i, package="gWidgets2")
                         }
                         icons <<- l
                       },
                       get_icon_from_name=function(nm) {
                         out <- sapply(nm, function(i) icons[[i]], simplify=FALSE)
                         if(length(out) == 1)
                           out[[1]]
                         else
                           out
                       },
                       set_icons = function(new_icons) icons <<- new_icons
                     ))
.gWidgetsIcons <- GWidgets2Icons$new()


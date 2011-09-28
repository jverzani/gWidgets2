##' A class to record the toolkit a gui object uses
##'
##' @exportClass guiWidgetsToolkit
setClass("guiWidgetsToolkit",
         representation(toolkit="character"),
         prototype(toolkit="")
         )


## Subclasses are done in the toolkit packages. For example, this is done in gWidgets2RGtk2:
## setClass("guiWidgetsToolkitRGtk2",
##          contains="guiWidgetsToolkit")


##################################################


##' set or get the current toolkit for gWidgets
##'
##' @param name name of toolkit (e.g. "tcltk", "RGtk2", "Qt" (not
##' qtbase)). If NULL, then we search for it in a) an iherited toolkit
##' object b) the "guiToolkit" option (which can be set via
##' \code{options("guiToolkit"="RGtk2")}, say. If that fails, we
##' prompt for a selection for any installed toolkit.  In the typical
##' usage, this all happens in the background, except perhaps once.
##'
##' In design this is to allow different toolkits to be used with
##' different GUIs, but due to differences in event loops, this often
##' leads to lockups, so is not recommended.
##' @return an instance of guiWidgetsToolkit sub class.
##' @export
guiToolkit <- function(name=NULL) {
  ## plan, if name is NULL, and options("guiToolkit") NULL then we popup a menu
  ## with choices coming from all installed packages named gWidgetsXXXX
  ## when a name is selected, we require the package gWidgets+name

  if(missing(name) || is.null(name)) {
    ## try to get from inheritance, then get from option

    x = try(get("toolkit", inherits=TRUE), silent=TRUE)
    if(is(x, "guiWidgetsToolkit")) {
      name <- x
    } else {
      name = getOption("guiToolkit")
    }
  }
  if(!is.null(name) && is.na(name)) {
    message("Choice overridden")
    return(NULL)          # use NA to override choice
  }

  
  ## no if it is null, we have to find the possible choices
  if(is.null(name)) {

    out <- system.time(poss_packages <- rownames(installed.packages()))
    if(out[3] > 1)
      message("You can speed this up by setting a toolkit. Searching with installed.packages() can be time consuming.")

    choices <- poss_packages[grepl("gWidgets2.", poss_packages)]


    
    if(interactive()) {
      if(length(choices) == 0) {
        message("No toolkit packages are installed. Opening browser to some details for that.")
        browseURL(sprintf("file://%s", system.file("install/installing_toolkits.html", package="gWidgets2")
        return(NULL)
      } else if(length(choices) == 1) {
        theChoice <- choices
      } else {
        theChoice <- menu(choices, title="Select a GUI toolkit")
        if(theChoice == 0) {
          warning("No toolkit loaded")
          return(NULL)
        } else {
          theChoice <- choices[theChoice]
        }
      }
      ## go with theChoice
      name <- gsub("^gWidgets2","",theChoice)
      options("guiToolkit"=name)

    } else {
      ## not interactive 
      return(NULL)
    }
  }

  ## override
  if(name == "qtbase")
    name <- "Qt"

  if(grepl("^2", name)) {
    message("Why is there a 2 in name")
    name <- gsub("^2", "", name)
  }

  
  ## require the package
  require(sprintf("gWidgets2%s", name, sep=""), character.only=TRUE)

  ## we return an instance of the toolkit class
  obj <- new(sprintf("guiWidgetsToolkit%s", name), toolkit = name)
  return(obj)
}

##' Which toolkit are we using?
##'
##' @return string of toolkit (RGtk2, tcltk, Qt, ???)
##' @export
gtoolkit <- function() {
  obj <- guiToolkit()
  obj@toolkit
}

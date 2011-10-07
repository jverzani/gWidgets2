##' @include methods.R
NULL

##' menu constructor, main and popup
##'
##' A menu may be viewed as a heirarchical collection of buttons, each
##' invoked by clicking on the button. These buttons are exposed
##' through submenus. More generally, a widget may replace the
##' button. This widget intends to support buttons (gactions),
##' separators (gseparator), radio button (gradio) and checkbutton
##' (gcheckbox).
##' @param menu.list A list defining the menu structure. Named sub
##' lists determine the submenu titles and structure. The list may
##' have components of class: \code{GAction}, mapped to a button;
##' \code{GSeparator}, mapped to a horizontal separator;
##' \code{GRadio}, mapped to linked buttons; or \code{GCheckbox},
##' mapped to a checkbox button.
##' @param popup logical. If true, make a popup window to be added through a handler call
##' @param container For non-popup menus, a top-level \code{GWindow} instance to place the menu bar within.
##' @param ... 
##' @param toolkit 
##' @export
gmenu <- function(
                  menu.list,
                  popup = FALSE,
                  container = NULL,      ... ,
                  toolkit=guiToolkit()){

  deprecated_args <- list(menulist="Use menu.list instead",
                          action="No action argument, parameterize gaction objects individually")
  check_deprecated(deprecated_args, ...)

 
  obj <- .gmenu(toolkit,
                menu.list=menu.list, popup=popup, container=container ,...
                )
  check_return_class(obj, "GMenuBar")
  return(obj)
}


##' generic for toolkit dispatch
##'
##' @export
##' @rdname gmenu
.gmenu <-  function(toolkit,
                    menu.list=list(),
                    popup=FALSE,
                    container = NULL,
                    ... )
  UseMethod( '.gmenu' )


## svalue, add, 


##' "svalue<-" method
##'
##' for a menubar, \code{svalue<-} replaces the menubar items with new ones specified by value.
##' @InheritParams "svalue<-"
##' @export
##' @rdname svalue
"svalue<-.GToolBar" <- function(obj, index=NULL, ..., value) NextMethod()

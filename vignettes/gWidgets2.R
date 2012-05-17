### R code from vignette source 'gWidgets2.Rnw'

###################################################
### code chunk number 1: setup
###################################################
options(width=72)
options(prompt='  ',continue='  ')  # remove prompt characters at start of lines


###################################################
### code chunk number 2: hello_world
###################################################
library(gWidgets2)
options(guiToolkit="RGtk2")
##
win <- gwindow("Basic example", visible=FALSE)
gp <- gvbox(container=win)
btn <- gbutton("click me for a message", container=gp)
##
addHandlerClicked(btn, handler=function(h,...) {
  galert("Hello world!", parent = win)
})
##
visible(win) <- TRUE


###################################################
### code chunk number 3: nested_container
###################################################
## Some filler
lorem <- "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
##
win <- gwindow("Nested groups")
g <- gvbox(container=win)
g$set_borderwidth(10L)
txt <- gtext(lorem, container=g, expand=TRUE, fill=TRUE) ## text widget
bg <- ggroup(cont=g)
addSpring(bg)
gbutton("dismiss", container=bg, handler=function(h,...) dispose(win))
gbutton("about", container=bg, handler=function(h,...) {
  gmessage("Shows lorem ipsum text", parent=win)
})


###################################################
### code chunk number 4: gformlayout_example
###################################################
win <- gwindow("t-test", visible=FALSE)
g <- gvbox(container=win)
g$set_borderwidth(10L)
flyt <- gformlayout(container=g, expand=TRUE)
## 
gedit("", initial.msg="variable", 
      label="x", container=flyt)
gcombobox(c("two.sided", "less", "greater"), 
          label="alternative",container=flyt)
gedit("0", coerce.with=as.numeric,  
      label="mu", container=flyt)
gcheckbox("", checked=FALSE, 
          label="paired", container=flyt)
gslider(from=0.5, to = 1.0, by=.01, value=0.95, 
        label="conf.level",  container=flyt)
##
bg <- ggroup(container=g)
addSpring(bg)
gbutton("values...", container=bg, handler=function(h,...) {
  print(svalue(flyt))                   # replace me...
})
addSpring(g)                            # better for Qt
##
size(win) <- c(400, 250)
visible(win) <- TRUE


###################################################
### code chunk number 5: pick_your_race
###################################################
win <- gwindow("handler example", visible=FALSE)
g <- gvbox(container=win)
f <- gframe("Ethnicity", container=g)
cb <- gcheckboxgroup(c("White", 
                  "American Indian and Alaska Native", 
                  "Asian", 
                  "Black or African American", 
                  "Native Hawaiian and Other Pacific Islander"),
                container=f)
bg <- ggroup(cont=g); addSpring(bg)
b <- gbutton("Go", container=bg)
enabled(b) <- FALSE
##
addHandlerChanged(cb, handler=function(h,...) {
  enabled(b) <- length(svalue(h$obj)) > 0
})
##
visible(win) <- TRUE


###################################################
### code chunk number 6: remove (eval = FALSE)
###################################################
## if(gconfirm(c("Remove x", "this can't be undone")))
##   rm("x")


###################################################
### code chunk number 7: about
###################################################
about <- "
A simple GUI to simplify the loading and unloading of packages.
This GUI uses `gcheckboxgroup`, with its `use.table` argument, to
present the user with familiar checkboxes to indicate selection.
Some indexing jujitsu is needed to pull out which value is checked to
trigger the event.
"


###################################################
### code chunk number 8: installed
###################################################
installed <- installed.packages() ## matrix
installed_packages <- installed[, "Package"]


###################################################
### code chunk number 9: gWidgets2.Rnw:548-553
###################################################
package_status <- function() {
  ## Return if package is loaded
  loaded <- loadedNamespaces()
  sapply(installed_packages, function(i) i %in% loaded)
}


###################################################
### code chunk number 10: gWidgets2.Rnw:560-563
###################################################
w <- gwindow("package manager", visible=FALSE)
g <- gvbox(cont=w)
g$set_borderwidth(10L)


###################################################
### code chunk number 11: gWidgets2.Rnw:572-576
###################################################
a <- package_status()
tbl <- gcheckboxgroup(installed_packages, checked=package_status(),
                      use.table=TRUE,
                      expand=TRUE, container=g)


###################################################
### code chunk number 12: gWidgets2.Rnw:586-600
###################################################
bg <- ggroup(cont=g)
addSpring(bg)
gbutton("About", container=bg, handler=function(...) {
  w1 <- gwindow("About", parent=w, visible=FALSE)
  g <- gvbox(container=w1); g$set_borderwidth(10)
  glabel(about, container=g)
  gseparator(container=g)
  bg <- ggroup(cont=g)
  addSpring(bg)
  gbutton("dismiss", cont=bg, handler=function(h,...) {
    dispose(w1)
  })
  visible(w1) <- TRUE
})


###################################################
### code chunk number 13: gWidgets2.Rnw:604-605
###################################################
visible(w) <- TRUE


###################################################
### code chunk number 14: gWidgets2.Rnw:612-618
###################################################
update_tbl <- function(...) {
  blockHandlers(tbl)
  on.exit(unblockHandlers(tbl))
  
  svalue(tbl, index=TRUE) <- package_status()
}


###################################################
### code chunk number 15: gWidgets2.Rnw:630-643
###################################################
addHandlerChanged(tbl, handler=function(h, ...) {
  ind <- svalue(h$obj, index=TRUE)
  old_ind <- which(package_status())

  if(length(x <- setdiff(old_ind, ind))) {
    message("detach ", installed_packages[x])
    pkg <- sprintf("package:%s", installed_packages[x])
    detach(pkg, unload=TRUE, character.only=TRUE)
  } else if (length(x <- setdiff(ind, old_ind))) {
    require(installed_packages[x], character.only=TRUE)
  }
  update_tbl()
})


###################################################
### code chunk number 16: gWidgets2.Rnw:670-671
###################################################
about <- "GUI to upgrade installed packages"


###################################################
### code chunk number 17: setCRAN
###################################################



###################################################
### code chunk number 18: gWidgets2.Rnw:681-686
###################################################
repos <- getOption("repos")
repos["CRAN"] <- "http://streaming.stat.iastate.edu/CRAN/"
options(repos = repos)
#
pkg <- old.packages()[,c("Package", "Installed", "ReposVer")]


###################################################
### code chunk number 19: gWidgets2.Rnw:691-694
###################################################
w <- gwindow("Upgrade installed packages", visible=FALSE)
g <- gvbox(container=w)
g$set_borderwidth(10)


###################################################
### code chunk number 20: gWidgets2.Rnw:699-704
###################################################

fg <- ggroup(container=g)
glabel("Filter by:", container=fg)
fltr <- gedit("", initial.msg="Filter by regexp", container=fg)
tbl <- gtable(pkg, chosen.col=1, multiple=TRUE, container=g, expand=TRUE)


###################################################
### code chunk number 21: gWidgets2.Rnw:708-717
###################################################
bg <- ggroup(container=g); addSpring(bg)
gbutton("About", container=bg, handler=function(h,...) {
  w1 <- gwindow("About", parent=w, visible=FALSE)
  g <- gvbox(container=w1); g$set_borderwidth(10)
  glabel(about, container=g, expand=TRUE)
  bg <- ggroup(container=g); addSpring(bg)
  gbutton("dismiss", container=bg, handler=function(h,...) dispose(w1))
  visible(w1) <- TRUE
})


###################################################
### code chunk number 22: gWidgets2.Rnw:724-735
###################################################
update_btn <- gbutton("Update selected", container=bg, handler=function(h,...) {
  pkgs <- svalue(tbl)
  if(length(pkgs) == 0) return()
  
  sapply(pkgs, install.packages)
  ## update pkg, then update talbe
  tbl[] <- pkg <<-  old.packages()[,c("Package", "Installed", "ReposVer")]
})
enabled(update_btn) <- FALSE
#
visible(w) <- TRUE


###################################################
### code chunk number 23: gWidgets2.Rnw:742-751
###################################################
addHandlerKeystroke(fltr, handler=function(h,...) {
  regexp <- svalue(h$obj)
  if(nchar(regexp) > 0 && regexp != "") {
    ind <- grepl(regexp, pkg[, 'Package'])
    visible(tbl) <- ind
  } else {
    visible(tbl) <- rep(TRUE, nrow(pkg))
  }
})


###################################################
### code chunk number 24: gWidgets2.Rnw:760-763
###################################################
addHandlerSelectionChanged(tbl, handler=function(h,...) {
  enabled(update_btn) <- length(svalue(h$obj))
})



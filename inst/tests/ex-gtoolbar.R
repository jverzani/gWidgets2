library(gWidgets2)
options(guiToolkit="RGtk2")


w <- gwindow("Tool and menu bar test")
## toolbar
h <- function(...) print("hi")
tblist <- list(quit=gaction("quit", icon="quit", handler=h),
               gseparator(),
               ## Can add other widgets
               cb = gcombobox(state.name, handler=function(h,...) print(svalue(h$obj))),
               rb = gradio(1:3, horizontal=TRUE, handler=function(h,...) print(svalue(h$obj)))
               )
tb <- gtoolbar(tblist, cont=w)

mblist <- list(File=list(
                 quit=gaction("quit", icon="quit", handler=h),
                 ok = gaction("ok", icon="ok", handler=h)
                 ),
               gseparator(),               
               Edit=list(
                 undo=gaction("undo", icon="undo", handler=h),
                 redo=gaction("redo", icon="redo", handler=h)
                 ),
               Otheres=list(
                 ## can add other widgets (radio, checkbox)
                 radio=gradio(state.name[1:3], handler=function(h,...) print(svalue(h$obj))),
                 gseparator(),
                 tb=gcheckbox("really", handler=function(h,...) print(svalue(h$obj)))
                 )
               )

## add a menu bar
mb <- gmenu(mblist, cont=w)

## how to add a popup menu
l <- glabel("Right click for popup", cont=w)
add3rdmousePopupMenu(l, mblist[[1]])

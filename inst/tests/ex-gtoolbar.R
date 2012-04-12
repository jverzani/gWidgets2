w <- gwindow("Tool and menu bar test")
## toolbar
h <- function(...) print("hi")
tblist <- list(quit=gaction("quit", icon="quit", handler=h, parent=w),
               gseparator(),
               help=gaction("help", icon="help", handler=h, parent=w)
               )
#tb <- gtoolbar(tblist, cont=w)

mblist <- list(File=list(
                 quit=gaction("quit", icon="quit", handler=h, parent=w),
                 ok = gaction("ok", icon="ok", handler=h, parent=w)
                 ),
               gseparator(),               
               Edit=list(
                 undo=gaction("undo", icon="undo", handler=h, parent=w),
                 redo=gaction("redo", icon="redo", handler=h, parent=w)
                 ),
               Otheres=list(
                 ## can add other widgets (radio, checkbox)
                 radio=gradio(state.name[1:3], parent=w, handler=function(h,...) print(svalue(h$obj))),
                 gseparator(),
                 tb=gcheckbox("really", parent=w, handler=function(h,...) print(svalue(h$obj)))
                 )
               )

## add a menu bar
mb <- gmenu(mblist, cont=w)

## how to add a popup menu
l <- gbutton("Right click for popup", cont=w)
add3rdmousePopupMenu(l, mblist[[1]])

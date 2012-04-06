## example of menu, tool, and status  bars.

ctr <- 1                                # some global

## Need a parent window
w <- gwindow("Example of bars", visible=FALSE)

f <- function(...) {ctr <<- ctr + 1; svalue(sb) <- sprintf("clicked %s times", ctr)}


## tcltk has checkbuttons and radio buttons added. Others are more general (comboboxes, ...)
## In the implementation actions are shared, but widgets may not be. That is, the checkbutton may not
## be synchronized in the menubar and the toolbar!
acts <- list(open=gaction("open", icon="open", tooltip="open", handler=f, parent=w),
             quit=gaction("quit", icon="close", tooltip="quit", handler=f, parent=w),
             checkbutton = gcheckbox("click me", handler=f, parent=w),
             radio = gradio(c("click", "me"), horizontal=FALSE, handler=f, parent=w)
             )
mb_list <- list(File=list(acts[[1]], gseparator(parent=w), acts[[2]],
                  SubMenu=list(acts[[1]])),
                Other=list(acts[[3]], gseparator(parent=w), acts[[4]]))

sb <- gstatusbar("clicked 0 times", cont=w)
tb <- gtoolbar(acts, cont=w)
mb <- gmenu(mb_list, cont=w)

g <- ggroup(cont=w, expand=TRUE)
visible(w) <- TRUE


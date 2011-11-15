if(interactive()) {

  w <- gwindow("Buttons", visible=FALSE)
  g <- ggroup(cont=w, horizontal=FALSE)

  ## various buttons

  ## with icons
  b1 <- gbutton("open", cont=g)

  ## without icon
  b2 <- gbutton("ouvrir", cont=g)

  ## by an action
  act <- gaction("open", tooltip="open", icon="open", handler=function(...) {})
  b3 <- gbutton(action=act, cont=g)

  ## with a handler
  b4 <- gbutton("click me", cont=g, handler=function(h,...) {
    if(svalue(b2) == "open")
      svalue(b2) <- "ouvrir"
    else
      svalue(b2) <- "open"
  })

  visible(w) <- TRUE
}

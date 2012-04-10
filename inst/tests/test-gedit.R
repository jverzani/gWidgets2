test.gedit <- function() {
  w <- gwindow()
  g <- ggroup(cont = w, horiz = FALSE)
  gbutton("Focus here", cont=g)
  
  text <- "label text"; newText <- "new"
  l <- gedit(text, cont = g)

  # svalue
  checkEquals(svalue(l), text)

  # svalue<-
  svalue(l) <- newText
  checkEquals(svalue(l), newText)

  l2 <- gedit("", initial="is this initial text? State name", cont=g)
  checkEquals(svalue(l2), "")
  # [<-
  l2[] <- state.name
  
}

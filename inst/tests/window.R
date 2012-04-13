w <- gwindow("windows and dialogs")
sb <- gstatusbar("hello", cont=w)
g <- ggroup(cont=w, horizontal=FALSE)


## test

## svalue
expect_equal(svalue(w), "windows and dialogs")
## svalue<-
svalue(w) <- "new title"
expect_equal(svalue(w), "new title")


## subwindow
gbutton("subwindow", cont=g, handler=function(h, ...) {
  w1 <- gwindow("Subwindow", parent=w)
  g <- ggroup(cont=w1, horizontal=FALSE)
  txt <- gtext("", cont=g, expand=TRUE)
  gbutton("dismiss", cont=g, handler=function(h,...) dispose(w1))
})


## dialogs

## alert
gbutton("alert", cont=g, handler=function(h,...) galert('alert', parent=w))


## messasge
gbutton("message", cont=g, handler=function(h, ...) {
  gmessage("message", parent=w)
})

## confirm
gbutton("confirm", cont=g, handler=function(h,...) {
  out <- gconfirm("Okay", parent=w)
  print(out)
})

## input
gbutton("input", cont=g, handler=function(h,...) {
  out <- ginput(msg="message", text="initial", title="title", parent=w)
  print(out)
})






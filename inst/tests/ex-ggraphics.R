library(gWidgets2)
if(gtoolkit() == 'RGtk2') {

  w <- gwindow("gimage")
  g <- ggroup(cont=w, horizontal=FALSE)
  gd <- ggraphics(cont=g, expand=TRUE, fill=TRUE)
  
  hist(rnorm(100))
  sl <- gslider(from=10, to=100, by=10, cont=g, handler=function(h,...) {
    hist(rnorm(svalue(h$obj)))
  })
  
  ## handler for clicking
  addHandlerClicked(gd, handler=function(h,...)  print(c(h$x, h$y)))
  
  ## rectangle select Returns h$x, h$y giving coordinates of rectangle
  addHandlerChanged(gd, handler=function(h,...)  print(c(h$x, h$y)))
}

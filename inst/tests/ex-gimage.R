library(gWidgets2)

## make a figure
f <- sprintf("%s.png",tempfile())
png(f)
hist(rnorm(100))
dev.off()

f1 <- sprintf("%s.png",tempfile())
png(f1)
hist(rnorm(100))
dev.off()

w <- gwindow("gimage")
g <- ggroup(cont=w, horizontal=FALSE)

## stock
img1 <- gimage(stock.id="ok", cont=g)

## replacement
svalue(img1) <- "cancel"

## file
img2 <- gimage(filename=basename(f), dirname=dirname(f), cont=g)


## replacement
svalue(img2) <- f1

## handler
addHandlerClicked(img1, handler=function(h,...)  print(str(h)))

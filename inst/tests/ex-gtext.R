w <- gwindow("gtext example", visible=FALSE)

## constrouctor -- font.attr sets for buffer
widget <- gtext("test text", cont = w, font.attr=c( color="blue"))
insert(widget, "new text", font.attr=c(size=24L, family="monospace"))

                
# svalue
print(svalue(widget))

# svalue<-
svalue(widget) <- "new label"

# font<-
# sets for buffer if no selection
font(widget) <- c(family="monospace", "weight"="bold", "color"="red", scale="xx-large")

visible(w) <- TRUE

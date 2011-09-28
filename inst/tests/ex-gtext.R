w <- gwindow("gtext example", visible=FALSE)

## constrouctor -- font.attr sets for buffer
widget <- gtext("test text", cont = w, font.attr=c(size=24L, color="blue"))
add(widget, "new text", font.attr=c(family="monospace"))

                
# svalue
print(svalue(widget))

# svalue<-
svalue(widget) <- "new label"

# font<-
# sets for buffer if no selection
font(widget) <- c(family="monospace", "weight"="bold", "color"="red", size="xx-large")

visible(w) <- TRUE

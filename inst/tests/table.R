w <- gwindow("gtable", visible=FALSE)
g <- ggroup(cont=w, horizontal=FALSE)

x <- data.frame("key"=state.name[1:10], value=state.x77[1:10,'Population'], stringsAsFactors=FALSE)
y <- x
y$icons <- rep("ok", length=10)
y$tooltips <- toupper(state.name[1:10])
##
tbl1 <- gtable(x, cont=g)
tbl2 <- gtable(y, icon.col=3, tooltip.col=4, cont=g)

visible(w) <- TRUE


## test
## svalue
expect_equal(svalue(tbl1), x[[1]][integer(0)])

## svalue<-
svalue(tbl1, index=TRUE) <- 1
expect_equal(svalue(tbl1, index=TRUE), 1)
expect_equal(svalue(tbl1, drop=TRUE), state.name[1])

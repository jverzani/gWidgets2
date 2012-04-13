w <- gwindow("test")
g <- ggroup(cont=w, horizontal=FALSE)

l1 <- glabel("one", cont=g)
l2 <- glabel("<b>one</b>", markup=TRUE, cont=g)

## tests
## svalue
expect_equal(svalue(l1), "one")

## svalue<-
svalue(l1) <- "two"
expect_equal(svalue(l1), "two")

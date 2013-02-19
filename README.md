## The `gWidgets2` package

The `gWidgets2` package is a rewrite of the `gWidgets` package that
allows R users to easily create graphical user interfaces (GUIs) from
within R in a toolkit-independent manner. The package itself sets up
an API for describing these interfaces. An accompanying package
integrates this into the underlying toolkit library.

Currently there are the following packages:

* `gWidgets2RGtk2` for interfacing with the `GTK` set of widgets through the `RGtk2` package. 

* `gWidgets2tcltk` for interfacing with the `TCL/TK` widgets through the `tcltk` package.

* `gWidgets2Qt` for interfacing with the `Qt` set of widget through the `qtbase`

Additionally, for web programming the packages `gWidgetsWWW2` and
`gWidgetsWWW2.rapache` implement basically the same API using the
ExtJs JavaScript libraries.

To use the package you need to have intalled: the underlying graphical
toolkit libraries, one of the basic R interfaces (`RGtk2`, `tcltk` or
`Qt`), then `gWidgets2` and one or more of the `gWidgets2XXX`
packages. The `tcltk` interface is usually the easiest, as the
underlying graphical toolkit libraries are bundled with the standard
windows binary of R, thought `RGtk2` and `Qt` versions work with a
much richer underlying set of controls.


The `gWidgets2` API exposes only a small subset of what is available
in the underlying toolkit, but does it in a fairly easy to learn way
using many of `R`'s standard methods, as possible.

A basic example of making a "hello world" app would be:

```
w <- gwindow("Hello...", visible=FALSE)       ## a parent container
g <- ggroup (cont = w)                        ## A box container
b <- gbutton("Click me for a message", cont=g, expand=TRUE)  ## some control

addHandlerClicked(b, function(...) {          ## adding a callback to an event
  gmessage("Hello world!", parent=w)          ## a dialog		    
})

visible(w) <- TRUE                            ## a method call
```

Though short, this example illustrates the range of basic tasks needed
to construct a GUI. Some other examples are in the `examples`
directory of the package and the package `vignette`.




## Why a rewrite of the `gWidgets` package?


* This should be much faster (early benchmarking is about 10-100% as
  fast). The gWidgets package had a cumbersome dispatch mechanism
  based on S4 methods. This rewrite uses a lighter-weight S3 dispatch
  and reference classes.

* Should be easier to maintain. The documentation is done with
  roxygen2. The old version was by hand. It required some tedious
  stuff, best left unsaid. 

* rethinking some of the argument names, functions etc.. I don't think
  we need +gcommandline+, +gformlayout+, +ggenericwidget+, etc. These
  are best left to a separate package, if at all. Most of the
  arguments were made to be consistent. There weren't major changes,
  but enough to warrant adding a "2" to the package name.

* The code in the toolkit implementations needed cleaning up. Lots of
  it. This rewrite gave reason to do so.

* putting more structure on the toolkit. Before the class structure
  was left alone, leaving open the possibility that it could be made
  better (by someone else). Well, it wasn't. By enforcing a class
  structure with specific method names in the toolkit packages the
  code becomes more consistent. It should also make it easier for users
  to derive subclasses

* adding in some other features. For example, `gprogressbar`, `gtimer`, `gstackwidget`, ...


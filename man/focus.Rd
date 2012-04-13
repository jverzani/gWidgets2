\name{focus}
\alias{focus}
\alias{focus.default}
\alias{focus<-}
\alias{focus<-.default}
\title{Does widget have focus}
\usage{
  focus(obj)

  \method{focus}{default} (obj)

  focus(obj) <- value

  \method{focus}{default} (obj, value) <- value
}
\arguments{
  \item{obj}{object}

  \item{value}{logical. Set focus state.}
}
\description{
  a widget has focus if it will receive input events

  Basic S3 method

  For some widgets, this sets user focus (e.g. gedit gets focus for typing). For others, settig the focus calls the raise methods. (gor gwindow, it will raise the window)

  Basic S3 method for focus
}


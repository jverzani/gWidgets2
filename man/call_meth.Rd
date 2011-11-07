\name{call_meth}
\alias{call_meth}
\title{helper function to bypass lack of cached value in method call}
\usage{
  call_meth(meth, obj)
}
\arguments{
  \item{meth}{method name}

  \item{obj}{method of object's class}
}
\value{
  the method
}
\description{
  helper function to bypass lack of cached value in method
  call
}
\note{
  use as do.call(call_meth, args)
}


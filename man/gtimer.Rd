\name{gtimer}
\alias{.gtimer}
\alias{gtimer}
\title{Basic timer widget}
\usage{
  gtimer(ms, FUN, data = NULL, one.shot = FALSE,
    start = TRUE, toolkit = guiToolkit())

  .gtimer(toolkit, ms, FUN, data = NULL, one.shot = FALSE,
    start = TRUE)
}
\arguments{
  \item{ms}{interval in milliseconds}

  \item{FUN}{FUnction to call. Has one argument, data
  passed in}

  \item{data}{passed to function}

  \item{one.shot}{logical. If TRUE, called just once, else
  repeats}

  \item{start}{logical. If FALSE, started by
  \code{start_timer} OO method. (Call
  \code{obj$start_time()}).}

  \item{toolkit}{gui toolkit to dispatch into}
}
\description{
  Calls FUN every ms/1000 seconds. A timer is stopped
  through its \code{stop_timer} method which is called
  using OO style: \code{obj$stop_timer()}.

  S3 generic for dispatch
}
\examples{
\dontrun{
i <- 0
FUN <- function(data) {i <<- i + 1; if(i > 10) a$stop_timer(); print(i)}
a <- gtimer(1000, FUN)
##
## a one shot timer is run only once
FUN <- function(data) message("Okay, I can breathe now")
hold_breath <- gtimer(1000*60, FUN, one.shot=TRUE)
}
}


\name{BasicToolkitInterface}
\alias{BasicToolkitInterface}
\title{Basic interface for a widget. These are methods referenced by the S3 methods}
\description{
  This interface is inherited by the base GComponent
  classes in the toolkit implementations. The methods
  defined here are referenced by the S3 methods. For
  exampe, \code{svalue} dispatches to \code{get_value}.
}
\details{
  We combine both widget and container methods here. It
  isn't perfect, but they do share quite a bit. Perhaps, we
  could make the container class subclass the basic
  interface.
}


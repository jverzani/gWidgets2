\name{[.GTree}
\alias{[.GTree}
\title{extract method for gtree}
\arguments{
  \item{i}{ignored}

  \item{j}{ignored}

  \item{drop}{logical, if TRUE then only the key is
  returned. (Similar to \code{svalue}, which is recommended
  in this case.). Otherwise the row data is.}
}
\description{
  The \code{[} method is used to return the data associated
  with a selected row. The \code{svalue} method returns the
  path or its endpoint, the \code{[} method returns the row
  data associated with the path.
}


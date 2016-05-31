% Please edit documentation in R/misc.R
\name{check_deprecated}
\alias{check_deprecated}
\title{Method to send message if any depreacted arguments are being used}
\usage{
check_deprecated(deprecated_args = list(), ...)
}
\arguments{
\item{deprecated_args}{named list of deprecated args}

\item{...}{named avlues}
}
\description{
Many arguments were deprecated due to various reasons. This is meant to ease porting of code.
}


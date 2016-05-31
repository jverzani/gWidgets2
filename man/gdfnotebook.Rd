% Please edit documentation in R/gdfnotebook.R
\name{gdfnotebook}
\alias{.gdfnotebook}
\alias{gdfnotebook}
\title{A notebook container for many \code{gdf} instances}
\usage{
gdfnotebook(items = NULL, container = NULL, ..., toolkit = guiToolkit())

.gdfnotebook(toolkit, items, container, ...)
}
\arguments{
\item{items}{data frame for initial page, when given}

\item{container}{parent container}

\item{...}{passed to \code{add} method of parent container}

\item{toolkit}{toolkit}
}
\description{
A notebook container for many \code{gdf} instances

S3 generic whose methods are implemented in the toolkit packages
}


\name{add}
\alias{add}
\alias{add.default}
\alias{delete}
\alias{delete.GContainer}
\title{Add a child object to parent container}
\usage{
  add(obj, child, expand = FALSE, fill = NULL, anchor = NULL, ...)

  \method{add}{default} (obj, child, expand = FALSE, fill = NULL, anchor = NULL, ...)

  delete(obj, child)

  \method{delete}{GContainer} (obj, child)
}
\arguments{
  \item{obj}{parent object}

  \item{child}{child widget}

  \item{expand}{NULL or logical. XXX}

  \item{fill}{NULL or logical. XXX}

  \item{anchor}{NULL or logical. XXX}

  \item{...}{passed on}
}
\description{
  Add packs in child objects into parent box containers

  Basic S3 method for add

  Delete may or may note remove a child. This is toolkit specific. It may also be tied up with garbage collection. To avoid that, keep a reference to the child object before deleting.

  Basic S3 method for add
}


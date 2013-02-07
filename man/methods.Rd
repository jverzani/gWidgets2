\name{addSpring}
\alias{addSpace}
\alias{addSpace.GContainer}
\alias{addSpring}
\alias{addSpring.GContainer}
\title{Add a spring to box containers}
\usage{
  addSpring(obj)

  \method{addSpring}{GContainer} (obj)

  addSpace(obj, value)

  \method{addSpace}{GContainer} (obj, value)
}
\arguments{
  \item{obj}{GContainer object}

  \item{value}{space in pixels to add}
}
\value{
  NULL

  NULL
}
\description{
  A spring will separate the children packed in the box
  container prior to the spring be added and those being
  added, pushing the two as far apart as the allocated
  space will allow.

  Add spring to GContainer class

  Inserts a specific amount of space between the previously
  packed child and next one.

  Add space to GContainer class
}


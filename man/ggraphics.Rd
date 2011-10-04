\name{ggraphics}
\alias{.ggraphics}
\alias{ggraphics}
\title{Constructor for an embeddable graphics device}
\usage{
  ggraphics(width = dpi * 6, height = dpi * 6, dpi = 75, ps
  = 12, handler = NULL, action = NULL, container = NULL,
  ..., toolkit = guiToolkit())

  .ggraphics(toolkit, width = dpi * 6, height = dpi * 6,
  dpi = 75, ps = 12, container = NULL, ...)
}
\arguments{
  \item{width}{width of device (pixels)}

  \item{height}{hieght of widget (pixels)}

  \item{dpi}{dots per inch}

  \item{ps}{pointsize}

  \item{handler}{handler}

  \item{action}{action}

  \item{container}{parent container}

  \item{...}{passed to add method}

  \item{toolkit}{}
}
\description{
  Some toolkits provide an embeddable graphics device. When
  this is the case, this widget provides same.

  generic for toolkit dispatch
}


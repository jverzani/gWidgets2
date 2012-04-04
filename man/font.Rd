\name{font}
\alias{font}
\alias{font.default}
\alias{font<-}
\alias{font<-.default}
\title{Returns font specification for widget, if available}
\usage{
  font(obj)

  \method{font}{default} (obj)

  font(obj) <- value

  \method{font}{default} (obj, value) <- value
}
\arguments{
  \item{obj}{object}

  \item{value}{The font specification is given in terms of
  a named vector or list where the names indicate a font
  attribute and the value a reasonable choice:

  \describe{

  \item{weight}{c("light", "normal", "medium", "bold",
  "heavy")}

  \item{style}{c("normal", "oblique", "italic")}

  \item{family}{c("sans", "helvetica", "times",
  "monospace")}

  \item{size}{an integer, say c(6,8,10,11,12,14,16,18,20,
  24,36,72)}

  \item{color (or foreground)}{One of colors()}

  \item{background}{One of colors()}

  \item{scale}{c("xx-large", "x-large", "large" , "medium",
  "small", "x-small", "xx-small")}

  }

  These are from Gtk's font specs, which though fairly
  standard, may not be totally supported in the other
  toolkits.}
}
\description{
  Returns font specification for widget, if available

  Basic S3 method for font

  Set font for a widget

  Basic S3 method for setting font
}


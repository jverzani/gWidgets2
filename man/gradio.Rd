\name{gradio}
\alias{.gradio}
\alias{[<-.GRadio}
\alias{gradio}
\title{Constructor for radio button widget}
\usage{
  gradio(items, selected = 1, horizontal = FALSE, handler =
  NULL, action = NULL, container = NULL, ..., toolkit =
  guiToolkit())

  .gradio(toolkit, items, selected = 1, horizontal = FALSE,
  handler = NULL, action = NULL, container = NULL, ...)
}
\arguments{
  \item{items}{items to select from}

  \item{selected}{index of initially selected item}

  \item{horizontal}{layout direction}

  \item{handler}{handler for toggle event}

  \item{action}{action}

  \item{container}{parent container}

  \item{...}{passed to add method of parent containers}

  \item{toolkit}{toolkit}

  \item{x}{object}

  \item{i}{ignored}

  \item{j}{ignored}

  \item{...}{ignored}

  \item{value}{vector of items to select from}
}
\description{
  Constructor for radio button widget

  Generic for method dispatch

  Check for repeated itesm
}


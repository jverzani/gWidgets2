\name{guiWidgetsToolkit-class}
\alias{BasicToolkitInterface-class}
\alias{Command-class}
\alias{CommandList-class}
\alias{CommandStack-class}
\alias{guiWidgetsToolkit-class}
\alias{GWidgets2Icons-class}
\alias{Handler-class}
\alias{Observable-class}
\alias{Observer-class}
\alias{WSWatcherModel-class}
\title{A class to record the toolkit a gui object uses}
\description{
  A class to record the toolkit a gui object uses

  Reference class to create an observer of an observable
  object

  Handler is a special observer with obj and actino passed
  as first argument

  Observable class sets up objects that can be observed.
  Inherited by template

  This interface is inherited by the base GComponent
  classes in the toolkit implementations. The methods
  defined here are referenced by the S3 methods. For
  exampe, \code{svalue} dispatches to \code{get_value}.

  Class for icons

  A reference class to create a model that monitors the
  global workspace. The class has method
  \code{update_state} and the "getting" methods
  \code{get_by_class}, \code{get_by_function} (filter),
  \code{get_changes}. Use with a \code{gtimer} instance to
  keep up to date with changes to the workspace.

  Class for commands. Has methods do, redo, undo

  Class for multple commands

  A list with ptr. delegates call of do or undo to
  appropriate command
}
\details{
  We combine both widget and container methods here. It
  isn't perfect, but they do share quite a bit. Perhaps, we
  could make the container class subclass the basic
  interface.
}


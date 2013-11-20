\name{itreemap}
\alias{itreemap}
\title{Interactieve versie van treemap}
\usage{
  itreemap(dtf = NULL, index = NULL, vSize = NULL,
    vColor = NULL, type = NULL, height = NULL,
    command.line.output = TRUE)
}
\arguments{
  \item{dtf}{a data.frame (\code{\link{treemap}}) If not
  provided, then the first data.frame in the global
  workspace is loaded.}

  \item{index}{index variables (up to four). See
  \code{\link{treemap}}.}

  \item{vSize}{name of the variable that determine the
  rectangle sizes.}

  \item{vColor}{name of the variable that determine the
  rectangle colors. See \code{\link{treemap}}.}

  \item{type}{treemap type. See \code{\link{treemap}}.}

  \item{height}{height of the plotted treemap in pixels}
}
\description{
  A treemap is a space-filling visualization of
  hierarchical structures. This function offers great
  flexibility to draw treemaps. Required is a data.frame
  (\code{dtf}) that contains one or more hierarchical index
  columns given by \code{index}, a column that determines
  the rectangle area sizes (\code{vSize}), and optionally a
  column that determines the rectangle colors
  (\code{vColor}). The way how rectangles are colored is
  determined by the argument \code{type}.
}
\examples{
\dontrun{
data(business)
itreemap(business)
}
}


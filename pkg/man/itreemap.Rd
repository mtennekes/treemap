\name{itreemap}
\alias{itreemap}
\title{Interactieve versie van treemap}
\usage{
  itreemap(dtf = NULL, height = NULL)
}
\arguments{
  \item{dtf}{a data.frame (\code{\link{treemap}) If not
  provided, then all data.frames in the global workspace
  are given as options in the drop-down list, with the
  first one loaded.}

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


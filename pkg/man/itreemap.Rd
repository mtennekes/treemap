\name{itreemap}
\alias{itreemap}
\title{Interactieve versie van treemap}
\usage{
  itreemap(dtf, index, vSize, vColor = NULL,
    type = "index")
}
\arguments{
  \item{dtf}{a data.frame. Required.}

  \item{index}{vector of column names in \code{dtf} that
  specify the aggregation indices. It could contain only
  one column name, which results in a treemap without
  hierarchy. If multiple column names are provided, the
  first name is the highest aggregation level, the second
  name the second-highest aggregation level, and so on.
  Required.}

  \item{vSize}{name of the column in \code{dtf} that
  specifies the sizes of the rectangles. Required.}

  \item{vColor}{name of the column that, in combination
  with \code{type}, determines the colors of the
  rectangles. The variable can be scaled by the addition of
  "*<scale factor>" or "/<scale factor>".}

  \item{type}{type of the treemap. See
  \code{\link{treemap}}}
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


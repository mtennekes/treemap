\name{tmPlot}
\alias{tmPlot}
\title{User-friendly treemap function}
\usage{
  tmPlot(dtf, index, vSize, vColor = "", sortID = "",
    type = "auto", titles = NA, subtitles = NA,
    saveTm = FALSE)
}
\arguments{
  \item{dtf}{a data.frame (required).}

  \item{index}{a character vector containing the column
  names in \code{dtf} that contain the indices (required).}

  \item{vSize}{character containing the formula of the
  variables that determine the sizes (required). For
  details about the syntax see below.}

  \item{vColor}{a character containing the formula of the
  variables that determine the colors (optional). For
  details about the syntax see below.}

  \item{sortID}{the name of the column in \code{dtf} on
  which the rectangles should be sorted (from top left to
  bottom right). To inverse the sorting order, use "-" in
  the prefix. Optional.}

  \item{type}{the type of the treemap (optional): \itemize{
  \item \code{auto} automatic determination of type
  (default setting) \item \code{dens} density treemap
  (dense areas get darker colors) \item \code{comp}
  comparison treemap (colors are used to compare variables)
  \item \code{perc} treemap (color variable is in
  percentages) \item \code{linked} each index has an own,
  distinctive, color (useful for small multiples)}}

  \item{titles}{A character vector containing the title(s)
  of the treemap(s) (optional). Use this for describing the
  sizes of the rectangles.}

  \item{subtitles}{A character vector containing the
  subtitle(s) of the treemap(s) (optional). Use this for
  describing the colors of the rectangles.}

  \item{saveTm}{Logical that determines whether the treemap
  is saved. If \code{TRUE}, a list is returned. This list
  can be used to locate a mouse click (see
  \code{\link{tmLocate}}).}
}
\value{
  If \code{saveTm==TRUE}, then a list is returned:
  \item{tm}{List with for each treemap a \code{data.frame}
  containing information about the rectangles}
  \item{nRow}{Number of rows in the treemap grid}
  \item{nCol}{Number of rows in the treemap grid}
}
\description{
  User-friendly treemap function
}
\details{
  For the arguments \code{vSize} and \code{vColor}, use the
  following formula syntax: \itemize{ \item one treemap
  \itemize{ \item \code{vSize = <variable name>} \item
  \code{vColor = <variable name>/<scale>*<variable name>}
  second part (after /), useful for density treemaps, is
  optional when treemap type is "linked", this formula has
  no use} \item multiple treemaps formulas for each treemap
  are seperated with +}
}


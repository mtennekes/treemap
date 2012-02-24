\name{tmPlot}
\alias{tmPlot}
\title{User-friendly treemap function}
\usage{
  tmPlot(dtf, index, vSize, vColor = "", sortID = "",
    type = "auto", titles = NA, subtitles = NA,
    palette = NA, vColorRange = NA, fontsize.title = 14,
    fontsize.data = 11, fontsize.legend = 12,
    lowerbound.cex.data = 0.4)
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
  distinctive, color (useful for small multiples) \item
  \code{value} treemap where values of the color variable
  are directly mapped to a color palette. By default a
  diverging color scale (Brewer's "RdYlGn") is used where
  negative values are red and positive green. By setting
  the parameters \code{palette} and \code{vColorRange} any
  color palette can be used.}}

  \item{titles}{A character vector containing the title(s)
  of the treemap(s) (optional). Use this for describing the
  sizes of the rectangles. Optional.}

  \item{subtitles}{A character vector containing the
  subtitle(s) of the treemap(s) (optional). Use this for
  describing the colors of the rectangles. Optional.}

  \item{palette}{Either a color palette or a name of a
  Brewer palette (see \code{display.brewer.all()})
  (optional)}

  \item{vColorRange}{Range of the color variable values
  that is mapped to \code{palette} (optional)}

  \item{fontsize.title}{maximum) font size of the title
  (optional)}

  \item{fontsize.data}{maximum font size of the data
  labeling (optional)}

  \item{fontsize.legend}{maximum font size of the legend
  (optional)}

  \item{lowerbound.cex.data}{number between 0 and 1 that
  indicates the minimum fontsize of the data labels: 0
  means draw all data labels, and 1 means only draw data
  labels if they fit at font size \code{fontsize.data}}
}
\value{
  A list is silently returned: \item{tm}{List with for each
  treemap a \code{data.frame} containing information about
  the rectangles} \item{nRow}{Number of rows in the treemap
  grid} \item{nCol}{Number of rows in the treemap grid}
  This list can be used to locate a mouse click (see
  \code{\link{tmLocate}}).
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


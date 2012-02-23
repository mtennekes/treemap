\name{baseTreemap}
\alias{baseTreemap}
\title{Plots a single treemap}
\usage{
  baseTreemap(dat, type = "fixed",
    width = convertWidth(unit(1, "npc"), "inches", valueOnly = TRUE),
    height = convertHeight(unit(1, "npc"), "inches", valueOnly = TRUE),
    neg = FALSE, legenda = TRUE, upperboundText = 0.8,
    lowerboundText = 0.4, forcePrint = FALSE,
    sizeTitle = "", colorTitle = "")
}
\arguments{
  \item{dat}{data frame containing the columns: \itemize{
  \item \code{values} numerical vector defining rectangle
  sizes \item \code{values2} numerical vector defining
  rectangle colors \item \code{sortInd} vector defining the
  order of the rectangles (from top left to bottom right)
  \item \code{index1}...\code{index<k>} one or more vectors
  defining the aggregation indices}}

  \item{type}{the type of the treemap (optional): \itemize{
  \item \code{auto} automatic determination of type
  (default setting) \item \code{dens} density treemap
  (dense areas get darker colors) \item \code{comp}
  comparison treemap (colors are used to compare variables)
  \item \code{perc} treemap (color variable is in
  percentages) \item \code{linked} each index has an own,
  distinctive, color (useful for small multiples)}}

  \item{width}{width in inches used to determine text sizes
  (optional)}

  \item{height}{height in inches used to determine text
  sizes (optional)}

  \item{neg}{boolean to determine whether color values are
  interpreted negatively (optional)}

  \item{legenda}{boolean, determines whether legenda should
  be plot (optional)}

  \item{upperboundText}{upperbound of the text sizes
  (optional)}

  \item{lowerboundText}{lowerbound of the text sizes
  (optional)}

  \item{forcePrint}{boolean, determines whether text label
  should be print, even if they don't fit (optional)}

  \item{sizeTitle}{main title (optional)}

  \item{colorTitle}{subtitle (optional)}
}
\description{
  This function that plots a single treemap. It is used by
  the more user-friedly function \code{\link{tmPlot}}.
}


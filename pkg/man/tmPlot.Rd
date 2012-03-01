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
  variables that determine the colors. For details about
  the syntax see below.}

  \item{sortID}{the name of the column in \code{dtf} on
  which the rectangles should be sorted (from top left to
  bottom right). To inverse the sorting order, use "-" in
  the prefix.}

  \item{type}{the type of the treemap: \itemize{ \item
  \code{auto} automatic determination of type (default
  setting) \item \code{dens} density treemap (dense areas
  get darker colors) \item \code{comp} comparison treemap
  (colors are used to compare variables) \item \code{perc}
  treemap (color variable is in percentages) \item
  \code{linked} each index has an own, distinctive, color
  (useful to compare small multiples) \item \code{value}
  treemap where values of the color variable are directly
  mapped to a color palette. By default a diverging color
  scale (Brewer's "RdYlGn") is used where negative values
  are red and positive green. By setting \code{palette} (in
  combination with \code{vColorRange}), any color palette
  can be used.}}

  \item{titles}{A character vector containing the title(s)
  of the treemap(s) (optional). Use this for describing the
  sizes of the rectangles.}

  \item{subtitles}{A character vector containing the
  subtitle(s) of the treemap(s) (optional). Use this for
  describing the colors of the rectangles.}

  \item{palette}{Either a color palette or a name of a
  Brewer palette (see \code{display.brewer.all()})}

  \item{vColorRange}{Range of the color variable values
  that is mapped to \code{palette}. Only applicable for
  \code{type="value"}.}

  \item{fontsize.title}{maximum) font size of the title}

  \item{fontsize.data}{maximum font size of the data
  labeling}

  \item{fontsize.legend}{maximum font size of the legend}

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
\examples{
# fictive structural business statistics (sbs) data of 2008 and 2009
data(sbsData)

# comparisson treemaps
tmPlot(sbsData, index=c("section", "subsection"), vSize="employees09", vColor="employees08", sortID="-size")

tmPlot(sbsData, index=c("section"), vSize="employees09", vColor="employees08", sortID="-size")


# four comparisson treemaps
tmPlot(sbsData, 
	   index="section", 
	   vSize="employees09+value added09+turnover09+salaries09",
	   vColor="employees08+value added08+turnover08+salaries08", 
	   sortID="-size")


# density treemaps
tmPlot(sbsData,
	   index=c("section", "subsection"),
	   vSize="turnover09",
	   vColor="employees09/1000*turnover09", 
	   sortID="-size")

tmPlot(sbsData,
	   index=c("section", "subsection"),
	   vSize="employees09",
	   vColor="turnover09/employees09", 
	   sortID="-size", 
	   palette=terrain.colors(8))


# linked treemaps
tmPlot(sbsData[sbsData$section=="Manufacturing",],
	   index="subsection",
	   vSize="income09+employees09+expenditures09+salaries09",
	   vColor="", 
	   sortID="-size")


# value treemap (aka Map of the Market)
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   sortID="-size", 
	   type="value", 
	   palette="RdBu")
}


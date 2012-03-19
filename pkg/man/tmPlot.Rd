\name{tmPlot}
\alias{tmPlot}
\title{User-friendly treemap function}
\usage{
  tmPlot(dtf, index, vSize, vColor = NULL,
    sortID = "-size", type = "value", titles = NA,
    subtitles = NA, palette = NA, vColorRange = NA,
    fontsize.title = 14, fontsize.labels = 11,
    fontsize.legend = 12, lowerbound.cex.labels = 0.4,
    inflate.labels = FALSE, force.print.labels = FALSE,
    na.rm = FALSE)
}
\arguments{
  \item{dtf}{a data.frame (required).}

  \item{index}{character vector containing the column names
  in \code{dtf} that contain the aggregation indices
  (required).}

  \item{vSize}{character vector of variable names (one for
  each treemap) that determine the sizes (required).}

  \item{vColor}{character vector of variable names (one for
  each treemap) that determine the colors. For details
  about the syntax see below.}

  \item{sortID}{character vector of variable names (one for
  each treemap) that determine the sorting order of the
  rectangles (from top left to bottom right). Also the
  values "size" and "color" can be used. To inverse the
  sorting order, use "-" in the prefix. By default, large
  rectangles are placed top left.}

  \item{type}{the type of the treemap: \describe{
  \item{\code{comp}:}{colors indicate change of the
  \code{vSize}-variable with respect to the
  \code{vColor}-variable (in percentages)}
  \item{\code{dens}:}{colors indicate density (like a
  population density map): \code{vColor} should be defined
  as something (e.g.\ population) per unit of \code{vSize}
  (e.g.\ area size)} \item{\code{perc}:}{the \code{vColor}
  variable should consist of percentages between 0 and
  100.} \item{\code{linked}:}{objects are linked by color
  over different treemaps} \item{\code{value}:}{the
  \code{vColor}-variable is directly mapped to a color
  palette (by default Brewer's diverging color palette
  "RdYlGn").}}}

  \item{titles}{A character vector containing the title(s)
  of the treemap(s). Use this for describing the sizes of
  the rectangles.}

  \item{subtitles}{A character vector containing the
  subtitle(s) of the treemap(s). Use this for describing
  the colors of the rectangles.}

  \item{palette}{Either a color palette or a name of a
  Brewer palette (see \code{display.brewer.all()}).}

  \item{vColorRange}{Range of the color variable values
  that is mapped to \code{palette}. Only applicable for
  \code{type=="value"}.}

  \item{fontsize.title}{(maximum) font size of the title}

  \item{fontsize.labels}{font size of the data labeling}

  \item{fontsize.legend}{(maximum) font size of the legend}

  \item{lowerbound.cex.labels}{number between 0 and 1 that
  indicates the minimum fontsize of the data labels: 0
  means draw all data labels, and 1 means only draw data
  labels if they fit at font size \code{fontsize.data}}

  \item{inflate.labels}{logical that determines whether
  data labels are inflated inside the rectangles}

  \item{force.print.labels}{logical that determines whether
  data labels are being forced to be printed (also when
  they don't fit)}

  \item{na.rm}{logical that determines whether missing
  values are omitted during aggregation}
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
  For the argument \code{vColor}, use the following formula
  syntax: \itemize{ \item one treemap \itemize{ \item
  \code{vSize = <variable name>} \item \code{vColor =
  <scale>*<variable name>} The second part (starting with
  /) is optional. This part is useful for density
  treemaps.} \item multiple treemaps: formulas are
  seperated with +}
}
\examples{

### fictive structural business statistics (sbs) data of 2008 and 2009
data(sbsData)


### treemap examples

# comparisson treemaps: colors indicate change of vSize with respect to vColor
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="comp")

tmPlot(sbsData, 
	   index=c("section"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="comp")

# four comparisson treemaps
tmPlot(sbsData, 
	   index="section", 
	   vSize=c("employees09", "value added09", "urnover09", "salaries09", 
	   vColor=c("employees08", "value added08", "turnover08", "salaries08",
	   type="comp")

# density treemaps: colors indicate density (like a population density map)
tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="turnover09",
	   vColor="0.001*employees09",
	   type="dens")

tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="employees09",
	   vColor="turnover09",
	   type="dens")

# linked treemaps: objects are linked by color over different treemaps
tmPlot(sbsData[sbsData$section=="Manufacturing",],
	   index="subsection",
	   vSize="income09", "employees09", "expenditures09", "salaries09",
	   vColor="",
	   type="linked")

# value treemap (aka Map of the Market)
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth",
	   type="value")


### graphical options: fontsize

# draw labels at fixed fontsize (fit only)
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="comp",
	   fontsize.labels=12, 
	   lowerbound.cex.labels=1)

# draw labels at flexible fontsize (skip tiny rectangles)
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   type="comp",
	   fontsize.labels=12, 
	   lowerbound.cex.labels=.6)

# draw labels at maximal fontsize
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   type="comp",
	   fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   inflate.labels = TRUE)

# draw all labels at fixed fontsize
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   type="comp",
	   fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   force.print.labels=TRUE)


### graphical options: color palette

# terrain colors
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value", 
	   palette=terrain.colors(10))

# Brewer's Red-White-Blue palette with predefined range
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value", 
	   palette="RdBu", 
	   vColorRange=c(-20000,20000))
}


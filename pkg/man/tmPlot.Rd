\name{tmPlot}
\alias{tmPlot}
\title{Create treemap}
\usage{
  tmPlot(dtf, index, vSize, vColor = NULL, type = "value",
    title = NA, subtitle = NA, algorithm = "pivotSize",
    sortID = "-size", palette = NA, vColorRange = NA,
    fontsize.title = 14, fontsize.labels = 11,
    fontsize.legend = 12, lowerbound.cex.labels = 0.4,
    inflate.labels = FALSE, force.print.labels = FALSE,
    na.rm = FALSE)
}
\arguments{
  \item{dtf}{a data.frame. Required.}

  \item{index}{vector containing the column names in
  \code{dtf} that contain the aggregation indices.
  Required.}

  \item{vSize}{name of the variable that determines the
  sizes of the rectangles. For small multiples, a vector of
  variable names (one for each treemap) should be given.
  Required.}

  \item{vColor}{name of the variable that, in combination
  with \code{type}, determines the colors of the
  rectangles. See \code{type} for more information. For
  small multiples, a vector of variable names (one for each
  treemap) should be given.}

  \item{type}{the type of the treemap: \describe{
  \item{\code{comp}:}{colors indicate change of the
  \code{vSize}-variable with respect to the
  \code{vColor}-variable (in percentages)}
  \item{\code{dens}:}{colors indicate density (E.g. a
  population density map: \code{vSize} is area size,
  \code{vColor} is population, and the colors are computed
  as densities (population per squared km's)).
  \code{vColor} can specified as "colName*scale factor" or
  "colName/scale factor" (see example below).}
  \item{\code{perc}:}{the \code{vColor} variable should
  consist of percentages between 0 and 100.}
  \item{\code{linked}:}{objects are linked by color over
  different treemaps} \item{\code{index}:}{each aggregation
  index has distinct color} \item{\code{value}:}{the
  \code{vColor}-variable is directly mapped to a color
  palette (by default Brewer's diverging color palette
  "RdYlGn").}}}

  \item{title}{Title of the treemap. For small multiples, a
  vector of titles should be given. Titles are used to
  describe the sizes of the rectangles.}

  \item{subtitle}{Subtitle of the treemap. For small
  multiples, a vector of subtitles should be given.
  Subtitles are used to describe the colors of the
  rectangles.}

  \item{algorithm}{name of the used algorithm: "squarified"
  or "pivotSize". The squarified treemap algorithm (Bruls
  et al., 2000) produces good aspect ratios, but ignores
  the sorting order of the rectangles (\code{sortID}). The
  ordered treemap algorithm, pivot-by-size (Bederson et
  al., 2002) takes the sroting order (\code{sortID}) into
  account while aspect ratios are acceptable.}

  \item{sortID}{name of the variable that determines the
  sorting order of the rectangles (from top left to bottom
  right). Also the values "size" and "color" can be used.
  To inverse the sorting order, use "-" in the prefix. By
  default, large rectangles are placed top left. For small
  multiples, a vector of variable names (one for each
  treemap) should be given. Only applicable when
  \code{algortihm=="pivotSize"}.}

  \item{palette}{Either a color palette or a name of a
  Brewer palette (see \code{display.brewer.all()}). A
  Brewer palette can be reversed by prefixing its name with
  a "-".}

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
  Create treemap
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

# four comparisson treemaps
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize=c("employees09", "value added09", "turnover09", "salaries09"), 
	   vColor=c("employees08", "value added08", "turnover08", "salaries08"),
	   type="comp")

# density treemaps: colors indicate density (like a population density map)
tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="turnover09",
	   vColor="employees09*0.001",
	   type="dens")

tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="employees09",
	   vColor="turnover09",
	   type="dens")

# linked treemaps: objects are linked by color over different treemaps
tmPlot(sbsData[sbsData$section=="Manufacturing",],
	   index="subsection",
	   vSize=c("income09", "employees09", "expenditures09", "salaries09"),
	   type="linked")

# index treemap: each aggregation index has distinct color
tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="employees09",
	   type="index")


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
\references{
  Bederson, B., Shneiderman, B., Wattenberg, M. (2002)
  Ordered and Quantum Treemaps: Making Effective Use of 2D
  Space to Display Hierarchies. ACM Transactions on
  Graphics, 21(4): 833-854. Bruls, D.M., C. Huizing, J.J.
  van Wijk. Squarified Treemaps. In: W. de Leeuw, R. van
  Liere (eds.), Data Visualization 2000, Proceedings of the
  joint Eurographics and IEEE TCVG Symposium on
  Visualization, 2000, Springer, Vienna, p. 33-42.
}


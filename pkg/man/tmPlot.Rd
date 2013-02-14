\name{tmPlot}
\alias{tmPlot}
\title{Create treemap}
\usage{
  tmPlot(dtf, index, vSize, vColor = NULL, type = "value",
    title = NA, subtitle = NA, algorithm = "pivotSize",
    sortID = "-size", palette = NA, range = NA,
    vColorRange = NULL, fontsize.title = 14,
    fontsize.labels = 11, fontsize.legend = 12,
    lowerbound.cex.labels = 0.4, inflate.labels = FALSE,
    bg.labels = ifelse(type \%in\% c("value", "linked", "categorical"), "#CCCCCCAA", NA),
    force.print.labels = FALSE,
    position.legend = ifelse(type \%in\% c("categorical", "index", "depth"), "right", ifelse(type == "linked", "none", "bottom")),
    aspRatio = NA, na.rm = FALSE)
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
  rectangles. The variable can be scaled by the addition of
  "*<scale factor>" or "/<scale factor>". For small
  multiples, a vector of variable names (one for each
  treemap) should be given.}

  \item{type}{the type of the treemap: \describe{
  \item{\code{"value"}:}{the \code{vColor}-variable is
  directly mapped to a color palette (by default Brewer's
  diverging color palette "RdBu").}
  \item{\code{"comp"}:}{colors indicate change of the
  \code{vSize}-variable with respect to the
  \code{vColor}-variable in percentages. Note: the negative
  scale may be different from the positive scale in order
  to compensate for the ratio distribution.}
  \item{\code{"dens"}:}{colors indicate density. This is
  aanalogous to a population density map where
  \code{vSize}-values are area sizes, \code{vColor}-values
  are populations per area, and colors are computed as
  densities (i.e.\ population per squared km's).}
  \item{\code{"linked"}:}{each object has a distinct color,
  which can be useful for small multiples (objects are
  linked by color)} \item{\code{"depth"}:}{each aggregation
  level (defined by \code{index}) has a distinct color}
  \item{\code{"index"}:}{colors are determined by the
  \code{index} variables. Each aggregation of the first
  index variable is assigned to a color of \code{palette}.
  Each aggregation of on of the other index variables is
  assigned to a similar color, that varies on hue or
  lightness.} \item{\code{"categorical"}:}{\code{vColor} is
  a categorical variable that determines the color}}}

  \item{title}{title of the treemap. For small multiples, a
  vector of titles should be given. Titles are used to
  describe the sizes of the rectangles.}

  \item{subtitle}{subtitle of the treemap. For small
  multiples, a vector of subtitles should be given.
  Subtitles are used to describe the colors of the
  rectangles.}

  \item{algorithm}{name of the used algorithm:
  \code{"squarified"} or \code{"pivotSize"}. The squarified
  treemap algorithm (Bruls et al., 2000) produces good
  aspect ratios, but ignores the sorting order of the
  rectangles (\code{sortID}). The ordered treemap,
  pivot-by-size, algorithm (Bederson et al., 2002) takes
  the sorting order (\code{sortID}) into account while
  aspect ratios are still acceptable.}

  \item{sortID}{name of the variable that determines the
  order in which the rectangles are placed from top left to
  bottom right. Also the values "size" and "color" can be
  used. To inverse the sorting order, use "-" in the
  prefix. By default, large rectangles are placed top left.
  For small multiples, a vector of variable names (one for
  each treemap) can be given. Only applicable when
  \code{algortihm=="pivotSize"}.}

  \item{palette}{either a color palette or a name of a
  Brewer palette (see \code{display.brewer.all()}). A
  Brewer palette can be reversed by prefixing its name with
  a "-".}

  \item{range}{range of values that determine the colors.
  When omitted, the range of actual values is used. This
  range is mapped to \code{palette}.}

  \item{vColorRange}{deprecated, use \code{range} instead.}

  \item{fontsize.title}{(maximum) font size of the title}

  \item{fontsize.labels}{font size(s) of the data labels,
  which can be: \itemize{ \item one number, which specifies
  the font size for all aggregation levels \item vector of
  two numbers, which specific the font sizes for 1) the
  highest and 2) the other aggregation levels \item vector
  of three numbers, which specific the font sizes for 1)
  the highest, 2) any in-between, and 3) the lowest
  aggregation level.}}

  \item{fontsize.legend}{(maximum) font size of the legend}

  \item{lowerbound.cex.labels}{multiplier between 0 and 1
  that sets the lowerbound for the data label font sizes: 0
  means draw all data labels, and 1 means only draw data
  labels if they fit at \code{fontsize.data}.}

  \item{inflate.labels}{logical that determines whether
  data labels are inflated inside the rectangles.}

  \item{bg.labels}{background color of labels of high
  aggregation levels. If set to \code{NA}, the color is
  determined by the color of the underlying rectangle. For
  value, categorical and linked treemaps, the default is
  transparent grey (\code{"#CCCCCCAA"}), and for the other
  types, \code{NA}.}

  \item{force.print.labels}{logical that determines whether
  data labels are being forced to be printed (also when
  they don't fit).}

  \item{position.legend}{position of the legend:
  \code{"bottom"}, \code{"right"}, or \code{"none"}. For
  categorical and index treemaps, \code{"right"} is the
  default value, for linked treemap, \code{"none"}, and for
  the other types, \code{"bottom"}.}

  \item{aspRatio}{preferred aspect ratio of the main
  rectangle, defined by width/height. When set to
  \code{NA}, the available window size is used.}

  \item{na.rm}{logical that determines whether missing
  values are omitted during aggregation}
}
\value{
  A list is silently returned: \item{tm}{list with for each
  treemap a \code{data.frame} containing information about
  the rectangles} \item{nRow}{number of rows in the treemap
  grid} \item{nCol}{number of rows in the treemap grid}
  \item{vSize}{argument vSize} \item{vColor}{argument
  vColor} This list can be used to locate a mouse click
  (see \code{\link{tmLocate}}).
}
\description{
  Create treemap
}
\examples{
#########################################
### quick example
###
### data: Gross national income data
#########################################

# load Gross national income data
data(GNI2010)

# create treemap
tmPlot(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")



#########################################
### extended examples
###
### data: fictive structural business statistics data
#########################################

### load fictive structural business statistics data
data(sbsData)
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08


#########################################
### types
#########################################

# value treemap: the color variable is directly mapped to the colors
tmPlot(sbsData, 
       index=c("section", "subsection"), 
       vSize="employees09", 
       vColor="employees.growth",
       type="value")

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
	   vColor="employees09*1000",
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

# depth treemap: each aggregation depth has distinct color
tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="employees09",
	   type="depth")

# index treemap: colors are determined by index
tmPlot(sbsData,
       index=c("section", "subsection"), 
       vSize="employees09",
       type="index")

# categorical treemap: colors are determined by a categorical variable
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="section",
	   type="categorical")

#########################################
### layout algorithm
#########################################

tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth",
	   type="value",
	   algorithm="squarified")

#########################################
### graphical options: fontsize
#########################################

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

#########################################
### graphical options: color palette
#########################################

# terrain colors
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value", 
	   palette=terrain.colors(10))

# Brewer's Red-White-Grey palette reversed with predefined range
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value", 
	   palette="-RdGy", 
	   range=c(-20000,20000))
}
\references{
  Bederson, B., Shneiderman, B., Wattenberg, M. (2002)
  Ordered and Quantum Treemaps: Making Effective Use of 2D
  Space to Display Hierarchies. ACM Transactions on
  Graphics, 21(4): 833-854.

  Bruls, D.M., C. Huizing, J.J. van Wijk. Squarified
  Treemaps. In: W. de Leeuw, R. van Liere (eds.), Data
  Visualization 2000, Proceedings of the joint Eurographics
  and IEEE TCVG Symposium on Visualization, 2000, Springer,
  Vienna, p. 33-42.
}


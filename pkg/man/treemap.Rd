\name{treemap}
\alias{treemap}
\title{Create treemap}
\usage{
  treemap(dtf, index, vSize, vColor = NULL, type = "index",
    title = NA, title.legend = NA, algorithm = "pivotSize",
    sortID = "-size", palette = NA,
    palette.HCL.options = list(hue_start = 30, hue_end = 390, hue_spread = TRUE, hue_fraction = 0.5, chroma = 60, luminance = 70, chroma_slope = 5, luminance_slope = -10),
    range = NA, fontsize.title = 14, fontsize.labels = 11,
    fontsize.legend = 12, lowerbound.cex.labels = 0.4,
    inflate.labels = FALSE,
    bg.labels = ifelse(type \%in\% c("value", "categorical"), "#CCCCCCAA", 220),
    force.print.labels = FALSE,
    position.legend = ifelse(type \%in\% c("categorical", "depth"), "right", ifelse(type == "index", "none", "bottom")),
    aspRatio = NA, vp = NULL, na.rm = TRUE)
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

  \item{vSize}{name of the variable in \code{dtf} that
  determines the sizes of the rectangles. Required.}

  \item{vColor}{name of the variable that, in combination
  with \code{type}, determines the colors of the
  rectangles. The variable can be scaled by the addition of
  "*<scale factor>" or "/<scale factor>".}

  \item{type}{the type of the treemap, which determines how
  the rectangles are colored: \describe{
  \item{\code{"index"}:}{colors are determined by the
  \code{index} variables. Different branches in the
  hierarchical tree get different colors. For this type,
  \code{vColor} is not needed.} \item{\code{"value"}:}{the
  \code{vColor}-variable is directly mapped to a color
  palette.} \item{\code{"comp"}:}{colors indicate change of
  the \code{vSize}-variable with respect to the
  \code{vColor}-variable in percentages. Note: the negative
  scale may be different from the positive scale in order
  to compensate for the ratio distribution.}
  \item{\code{"dens"}:}{colors indicate density. This is
  analogous to a population density map where
  \code{vSize}-values are area sizes, \code{vColor}-values
  are populations per area, and colors are computed as
  densities (i.e. population per squared km).}
  \item{\code{"depth"}:}{each aggregation level (defined by
  \code{index}) has a distinct color}
  \item{\code{"categorical"}:}{\code{vColor} is a
  categorical variable that determines the color}
  \item{\code{"color"}:}{\code{vColor} is a vector of
  colors in the hexadecimal (#RRGGBB) format}}}

  \item{title}{title of the treemap.}

  \item{title.legend}{title of the legend.}

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
  bottom right. Only applicable when
  \code{algortihm=="pivotSize"}. Also the values "size" and
  "color" can be used, which refer to \code{vSize} and
  \code{vColor} respectively. To inverse the sorting order,
  use "-" in the prefix. By default, large rectangles are
  placed top left.}

  \item{palette}{one of the following: #' \describe{
  \item{a color palette:}{i.e., a vector of hexadecimal
  colors (#RRGGBB)} \item{a name of a Brewer palette:}{See
  \code{RColorBrewer::display.brewer.all()} for the
  options. The palette can be reversed by prefixing with a
  "-". For treemap types "value" and "comp", a diverging
  palette should be chosen (default="RdYlGn"), for type
  "dens" a sequential (default="OrRd"). The default value
  for "depth" is "Set2".} \item{"HCL":}{Colors are derived
  from the Hue-Chroma-Luminance color space model. This is
  only applicable for qualitative palettes, which are
  applied to the treemap types "index", "depth", and
  "categorical". For "index" and "categorical" this is the
  default value.}}}

  \item{palette.HCL.options}{list of advanced options to
  pick colors from the HCL space (when
  \code{palette="HCL"}). This list contains: \describe{
  \item{\code{hue_start}:}{number between 0 and 360 that
  determines the starting hue value (default: 30)}
  \item{\code{hue_end}:}{number between \code{hue_start}
  and \code{hue_start + 360} that determines the ending hue
  value (default: 390)} \item{\code{hue_spread}:}{boolean
  that determines whether the colors are spread such that
  adjacent levels get more distinguishable colors. If
  \code{FALSE}, then the colors are equally distributed
  from \code{hue_start} to \code{hue_end} (default: TRUE)}
  \item{\code{hue_fraction}:}{number between 0 and 1 that
  determines the fraction of the hue circle that is used
  for recursive color picking: if 0 then the full hue
  circle is used, which means that the hue of the colors of
  lower-level nodes are spread maximally. If 1, then the
  hue of the colors of lower-level nodes are identical of
  the hue of their parents. (default: .5)}
  \item{\code{chroma}:}{chroma value of colors of the
  first-level nodes, that are determined by the first index
  variable (default: 60)}
  \item{\code{luminance}:}{luminance value of colors of the
  first-level nodes, i.e. determined by the first index
  variable (default: 70)} \item{\code{chroma_slope}:}{slope
  value for chroma of the non-first-level nodes. The chroma
  values for the second-level nodes are
  \code{chroma+chroma_slope}, for the third-level nodes
  \code{chroma+2*chroma_slope}, etc. (default: 5)}
  \item{\code{luminance_slope}:}{slope value for luminance
  of the non-first-level nodes (default: -10)}}}

  \item{range}{range of values that determine the colors.
  Only applicable for types "value", "comp", and "dens".
  When omitted, the range of actual values is used. This
  range is mapped to \code{palette}.}

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
  labels if they fit (given \code{fontsize.labels}).}

  \item{inflate.labels}{logical that determines whether
  data labels are inflated inside the rectangles. If TRUE,
  \code{fontsize.labels} is does not determine the maximum
  fontsize, but it does determine in combination with
  \code{lowerbound.cex.labels} the minimum fontsize.}

  \item{bg.labels}{background of labels of high aggregation
  levels. Either a color, or a number between 0 and 255
  that determines the transparency of the labels. In the
  latter case, the color itself is determined by the color
  of the underlying rectangle. For "value" and
  "categorical" treemaps, the default is transparent grey
  (\code{"#CCCCCCAA"}), and for the other types slightly
  transparent: \code{220}.}

  \item{force.print.labels}{logical that determines whether
  data labels are being forced to be printed (also when
  they don't fit).}

  \item{position.legend}{position of the legend:
  \code{"bottom"}, \code{"right"}, or \code{"none"}. For
  "categorical" and "index" treemaps, \code{"right"} is the
  default value, for "index" treemap, \code{"none"}, and
  for the other types, \code{"bottom"}.}

  \item{aspRatio}{preferred aspect ratio of the main
  rectangle, defined by width/height. When set to
  \code{NA}, the available window size is used.}

  \item{vp}{\code{\link[grid:viewport]{viewport}} to draw
  in. By default it is not specified, which means that a
  new plot is created. Useful when drawing small multiples,
  or when placing a treemap in a custom grid based plot.}

  \item{na.rm}{logical that determines whether missing
  values are omitted during aggregation}
}
\value{
  A list is silently returned: \item{tm}{a
  \code{data.frame} containing information about the
  rectangles} \item{vSize}{argument vSize}
  \item{vColor}{argument vColor}
}
\description{
  A treemap is a space-filling visualization of
  hierarchical structures. This package offers great
  flexibility to draw treemaps. Required is a data.frame
  (\code{dtf}) that contains one or more hierarchical index
  columns given by \code{index}, a column that determines
  the rectangle area sizes (\code{vSize}), and optionally a
  column that determines the rectangle colors
  (\code{vColor}). The way how rectangles are colored is
  determined by the argument \code{type}. #'
}
\examples{
#########################################
### quick example
###
### data: Gross national income data
#########################################
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
### data: fictive business statistics data
#########################################
data(business)

#########################################
### treemap types
#########################################

# index treemap: colors are determined by the index argument
tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       type="index")

# value treemap: the color variable is directly mapped to the colors
business <- transform(business, x = 1)

tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="x", 
       vColor="x",
       type="value")

# comparisson treemaps: colors indicate change of vSize with respect to vColor
tmPlot(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   vColor="employees.prev",
	   type="comp")

# density treemaps: colors indicate density (like a population density map)
tmPlot(business,
	   index=c("NACE1", "NACE2"), 
	   vSize="turnover",
	   vColor="employees*1000",
	   type="dens")

tmPlot(business,
	   index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
	   vSize="employees",
	   vColor="turnover",
	   type="dens")

# linked treemaps: objects are linked by color over different treemaps
tmPlot(business[business$NACE1=="C - Manufacturing",],
	   index=c("NACE2", "NACE3"),
	   vSize=c("employees"),
	   type="index")

# depth treemap: each aggregation depth has distinct color
tmPlot(business,
	   index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
	   vSize="employees",
	   type="depth")


# categorical treemap: colors are determined by a categorical variable
business$data.available <- factor(!is.na(business$turnover))
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
	   vSize="x", 
	   vColor="data.available",
	   type="categorical")

#########################################
### layout algorithm
#########################################

tmPlot(business, 
	   index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
	   vSize="employees", 
	   algorithm="squarified")

#########################################
### graphical options: fontsize
#########################################

# draw labels at fixed fontsize 12 (if they fit)
tmPlot(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   fontsize.labels=12, 
	   lowerbound.cex.labels=1)

# draw labels at fontsize (.6*12) to 12 (if they fit)
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       fontsize.labels=12, 
	   lowerbound.cex.labels=.6)

# draw all labels at maximal fontsize
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
	   lowerbound.cex.labels=0, 
	   inflate.labels = TRUE)

# draw all labels at fixed fontsize, even if they don't fit
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   force.print.labels=TRUE)

#########################################
### graphical options: color palette
#########################################


## for comp and value typed treemaps all diverging brewer palettes can be chosen
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       vColor="employees.prev",
       type="comp",
       palette="RdBu")

palette.HCL.options <- list(hue_start=30, hue_end=390, hue_spread=TRUE,
                            hue_fraction=0.5, chroma=60, luminance=70, 
                            chroma_slope=5, luminance_slope=-10)


tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       type="index",
       palette="Set3")


# terrain colors
business$employees.growth <- business$employees - business$employees.prev
tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       vColor="employees.growth", 
	   type="value", 
	   palette=terrain.colors(10))

# Brewer's Red-White-Grey palette reversed with predefined range
tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       vColor="employees.growth", 
	   type="value", 
	   palette="-RdGy", 
	   range=c(-30000,30000))
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


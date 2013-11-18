\name{treemap}
\alias{treemap}
\title{Treemap}
\usage{
  treemap(dtf, index, vSize, vColor = NULL, type = "index",
    title = NA, title.legend = NA, algorithm = "pivotSize",
    sortID = "-size", palette = NA,
    palette.HCL.options = NULL, range = NA,
    fontsize.title = 14, fontsize.labels = 11,
    fontsize.legend = 12,
    fontface.labels = c("bold", rep("plain", length(index) - 1)),
    fontfamily.title = "sans", fontfamily.labels = "sans",
    fontfamily.legend = "sans", border.col = "black",
    border.lwds = c(length(index) + 1, (length(index) - 1):1),
    lowerbound.cex.labels = 0.4, inflate.labels = FALSE,
    bg.labels = NULL, force.print.labels = FALSE,
    overlap.labels = 0.5,
    align.labels = c("center", "center"), xmod.labels = 0,
    ymod.labels = 0, position.legend = NULL,
    drop.unused.levels = TRUE, aspRatio = NA, vp = NULL)
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

  \item{type}{type of the treemap, which determines how the
  rectangles are colored: \describe{
  \item{\code{"index"}:}{colors are determined by the
  \code{index} variables. Different branches in the
  hierarchical tree get different colors. For this type,
  \code{vColor} is not needed.} \item{\code{"value"}:}{the
  numeric \code{vColor}-column is directly mapped to a
  color palette. This palette is diverging, so that values
  of 0 are assigned to the mid color (white or yellow), and
  negative and positive values are assigned to color based
  on two different hues colors (by default reds for
  negative and greens for positive values). For more
  freedom, see \code{"manual"}.}
  \item{\code{"comp"}:}{colors indicate change of the
  \code{vSize}-column with respect to the numeric
  \code{vColor}-column in percentages. Note: the negative
  scale may be different from the positive scale in order
  to compensate for the ratio distribution.}
  \item{\code{"dens"}:}{colors indicate density. This is
  analogous to a population density map where
  \code{vSize}-values are area sizes, \code{vColor}-values
  are populations per area, and colors are computed as
  densities (i.e. population per squared km).}
  \item{\code{"depth"}:}{each aggregation level (defined by
  \code{index}) has a distinct color. For this type,
  \code{vColor} is not needed.}
  \item{\code{"categorical"}:}{\code{vColor} is a factor
  column that determines the color.}
  \item{\code{"color"}:}{\code{vColor} is a vector of
  colors in the hexadecimal (#RRGGBB) format}
  \item{\code{"manual"}:}{The numeric \code{vColor}-column
  is directly mapped to a color palette. Both palette and
  range should be provided. The palette is mapped linearly
  to the range.}}}

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
  \code{algorithm=="pivotSize"}. Also the values "size" and
  "color" can be used, which refer to \code{vSize} and
  \code{vColor} respectively. To inverse the sorting order,
  use "-" in the prefix. By default, large rectangles are
  placed top left.}

  \item{palette}{one of the following: \describe{ \item{a
  color palette:}{i.e., a vector of hexadecimal colors
  (#RRGGBB)} \item{a name of a Brewer palette:}{See
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
  for recursive color picking: if 1 then the full hue
  circle is used, which means that the hue of the colors of
  lower-level nodes are spread maximally. If 0, then the
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
  of the non-first-level nodes (default: -10)}} For "depth"
  and "categorical" types, only the first two items are
  used.}

  \item{range}{range of values that determine the colors.
  Only applicable for types "value", "comp", and "dens".
  When omitted, the range of actual values is used. This
  range is mapped to \code{palette}.}

  \item{fontsize.title}{font size of the title}

  \item{fontsize.labels}{font size(s) of the data labels,
  which is either a single number that specifies the font
  size for all aggregation levels, or a vector that
  specifies the font size for each aggregation level}

  \item{fontsize.legend}{font size for the legend}

  \item{fontface.labels}{either a single value, or a vector
  of values one for each aggregation level. Values can be
  integers If an integer, following the R base graphics
  standard: 1 = plain, 2 = bold, 3 = italic, 4 = bold
  italic, or characters: \code{"plain"}, \code{"bold"},
  \code{"italic"}, \code{"oblique"}, and
  \code{"bold.italic"}.}

  \item{fontfamily.title}{font family of the title.
  Standard values are "serif", "sans", "mono", "symbol".
  Mapping is device dependent.}

  \item{fontfamily.labels}{font family of the labels in
  each rectangle. Standard values are "serif", "sans",
  "mono", "symbol". Mapping is device dependent.}

  \item{fontfamily.legend}{font family of the legend.
  Standard values are "serif", "sans", "mono", "symbol".
  Mapping is device dependent.}

  \item{border.col}{colour of borders drawn around each
  rectangle. Either one colour for all rectangles or a
  vector of colours, or one for each aggregation level}

  \item{border.lwds}{thicknesses of border lines. Either
  one number specifies the line thicknesses (widths) for
  all rectangles or a vector of line thicknesses for each
  aggregation level.}

  \item{lowerbound.cex.labels}{multiplier between 0 and 1
  that sets the lowerbound for the data label font sizes: 0
  means draw all data labels, and 1 means only draw data
  labels if they fit (given \code{fontsize.labels}).}

  \item{inflate.labels}{logical that determines whether
  data labels are inflated inside the rectangles. If TRUE,
  \code{fontsize.labels} is does not determine the maximum
  fontsize, but it does determine in combination with
  \code{lowerbound.cex.labels} the minimum fontsize.}

  \item{bg.labels}{background color of high aggregation
  labels. Either a color, or a number between 0 and 255
  that determines the transparency of the labels. In the
  latter case, the color itself is determined by the color
  of the underlying rectangle. For "value" and
  "categorical" treemaps, the default is (slightly)
  transparent grey (\code{"#CCCCCCDC"}), and for the other
  types slightly transparent: \code{220}.}

  \item{force.print.labels}{logical that determines whether
  data labels are being forced to be printed if they don't
  fit.}

  \item{overlap.labels}{number between 0 and 1 that
  determines the tolerance of the overlap between labels. 0
  means that labels of lower levels are not printed if
  higher level labels overlap, 1 means that labels are
  always printed. In-between values, for instance the
  default value .5, means that lower level labels are
  printed if other labels do not overlap with more than .5
  times their area size.}

  \item{align.labels}{object that specifies the alignment
  of the labels. Either a character vector of two values
  specifying the horizontal alignment (\code{"left"},
  \code{"center"}, \code{"right"}) and the vertical
  alignment (\code{"top"}, \code{"center"},
  \code{"bottom"}), or a list of sush character vectors,
  one for each aggregation level.}

  \item{xmod.labels}{the horizontal position modification
  of the labels in inches. Either a single value, or a
  vector that specifies the modifitaion for each
  aggregation level.}

  \item{ymod.labels}{the vertical position modification of
  the labels in inches. Either a single value, or a vector
  that specifies the modifitaion for each aggregation
  level.}

  \item{position.legend}{position of the legend:
  \code{"bottom"}, \code{"right"}, or \code{"none"}. For
  "categorical" and "index" treemaps, \code{"right"} is the
  default value, for "index" treemap, \code{"none"}, and
  for the other types, \code{"bottom"}.}

  \item{drop.unused.levels}{logical that determines whether
  unused levels (if any) are shown in the legend.
  Applicable for "categorical" treemap type.}

  \item{aspRatio}{preferred aspect ratio of the main
  rectangle, defined by width/height. When set to
  \code{NA}, the available window size is used.}

  \item{vp}{\code{\link[grid:viewport]{viewport}} to draw
  in. By default it is not specified, which means that a
  new plot is created. Useful when drawing small multiples,
  or when placing a treemap in a custom grid based plot.}
}
\value{
  A list is silently returned: \item{tm}{a
  \code{data.frame} containing information about the
  rectangles} \item{type}{argument type}
  \item{vSize}{argument vSize} \item{vColor}{argument
  vColor} \item{algorithm}{argument algorithm}
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
\examples{
#########################################
### quick example with Gross National Income data
#########################################
data(GNI2010)
treemap(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")

#########################################
### extended examples with fictive business statistics data
#########################################
data(business)

#########################################
### treemap types
#########################################

# index treemap: colors are determined by the index argument
\dontrun{
# large example which takes some time...
treemap(business, 
        index=c("NACE1", "NACE2", "NACE3"), 
        vSize="turnover", 
        type="index")
}
treemap(business[business$NACE1=="C - Manufacturing",],
        index=c("NACE2", "NACE3"),
        vSize=c("employees"),
        type="index")

# comparisson treemaps: colors indicate change of vSize with respect to vColor
treemap(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   vColor="employees.prev",
	   type="comp")

# density treemaps: colors indicate density (like a population density map)
treemap(business,
	   index=c("NACE1", "NACE2"), 
	   vSize="turnover",
	   vColor="employees/1000",
	   type="dens")

# categorical treemap: colors are determined by a categorical variable
business <- transform(business, data.available = factor(!is.na(turnover)), 
                      x = 1)
treemap(business, 
       index=c("NACE1", "NACE2"), 
	   vSize="x", 
	   vColor="data.available",
	   type="categorical")

#########################################
### graphical options: control fontsizes
#########################################

\dontrun{
# draw labels at fixed fontsize 12 (if they fit)
treemap(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   fontsize.labels=12, 
	   lowerbound.cex.labels=1)

# draw labels at fontsize (.6*12) to 12 (if they fit)
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       fontsize.labels=12, 
	   lowerbound.cex.labels=.6)

# draw all labels at maximal fontsize
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
	   lowerbound.cex.labels=0, 
	   inflate.labels = TRUE)

# draw all labels at fixed fontsize, even if they don't fit
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   force.print.labels=TRUE)

#########################################
### graphical options: color palettes
#########################################

## for comp and value typed treemaps all diverging brewer palettes can be chosen
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       vColor="employees.prev",
       type="comp",
       palette="RdBu")

## index treemaps, with palette="HCL":
palette.HCL.options <- list(hue_start=270, hue_end=360+150)
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       type="index",
       palette="HCL",
       palette.HCL.options=palette.HCL.options)

# terrain colors
business$employees.growth <- business$employees - business$employees.prev
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       vColor="employees.growth", 
	   type="value", 
	   palette=terrain.colors(10))

# Brewer's Red-White-Grey palette reversed with predefined range
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       vColor="employees.growth", 
	   type="value", 
	   palette="-RdGy", 
	   range=c(-30000,30000))
}
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


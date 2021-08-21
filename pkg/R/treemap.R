#' Create a treemap
#'
#' A treemap is a space-filling visualization of hierarchical structures. This function offers great flexibility to draw treemaps. Required is a data.frame (\code{dtf}) that contains one or more hierarchical index columns given by \code{index}, a column that determines the rectangle area sizes (\code{vSize}), and optionally a column that determines the rectangle colors (\code{vColor}). The way how rectangles are colored is determined by the argument \code{type}.
#' 
#' @param dtf a data.frame. Required.
#' @param index    vector of column names in \code{dtf} that specify the aggregation indices. It could contain only one column name, which results in a treemap without hierarchy. If multiple column names are provided, the first name is the highest aggregation level, the second name the second-highest aggregation level, and so on. Required. 
#' @param vSize name of the column in \code{dtf} that specifies the sizes of the rectangles. Required.
#' @param vColor name of the column that, in combination with \code{type}, determines the colors of the rectangles. The variable can be scaled by the addition of "*<scale factor>" or "/<scale factor>". Note: when omitted for \code{"value"} treemaps, a contant value of 1 is taken.
#' @param stdErr name of the column that contains standard errors. These are not used for the treemaps, but only aggregated accordingly and returned as item of the output list.
#' @param type type of the treemap, which determines how the rectangles are colored:
#' \describe{
#'    	\item{\code{"index"}:}{colors are determined by the \code{index} variables. Different branches in the hierarchical tree get different colors. For this type, \code{vColor} is not needed.}
#'    	\item{\code{"value"}:}{the numeric \code{vColor}-column is directly mapped to a color palette. This palette is diverging, so that values of 0 are assigned to the mid color (white or yellow), and negative and positive values are assigned to color based on two different hues colors (by default reds for negative and greens for positive values). For more freedom, see \code{"manual"}.}
#'		\item{\code{"comp"}:}{colors indicate change of the \code{vSize}-column with respect to the numeric \code{vColor}-column in percentages. Note: the negative scale may be different from the positive scale in order to compensate for the ratio distribution.}
#'		\item{\code{"dens"}:}{colors indicate density. This is analogous to a population density map where \code{vSize}-values are area sizes, \code{vColor}-values are populations per area, and colors are computed as densities (i.e. population per squared km).}
#'		\item{\code{"depth"}:}{each aggregation level (defined by \code{index}) has a distinct color. For this type, \code{vColor} is not needed.}
#'    	\item{\code{"categorical"}:}{\code{vColor} is a factor column that determines the color.}
#'      \item{\code{"color"}:}{\code{vColor} is a vector of colors in the hexadecimal (#RRGGBB) format}
#'      \item{\code{"manual"}:}{The numeric \code{vColor}-column is directly mapped to a color palette. Both palette and range should be provided. The palette is mapped linearly to the range.}}
#' @param fun.aggregate aggregation function, only used in \code{"value"} treemaps. This function determines how values of the lowest aggregation level are aggregated. By default, it takes the \code{sum}. Other sensible functions are \code{mean} and \code{weighted.mean}. In the latter case, the weights are determined by the \code{vSize} variable. Other arguments can be passed on. For \code{weighted.mean}, it is possible to assign a variable name for its \code{w} argument.
#' @param title title of the treemap.
#' @param title.legend title of the legend.
#' @param algorithm name of the used algorithm: \code{"squarified"} or \code{"pivotSize"}. The squarified treemap algorithm (Bruls et al., 2000) produces good aspect ratios, but ignores the sorting order of the rectangles (\code{sortID}). The ordered treemap, pivot-by-size, algorithm (Bederson et al., 2002) takes the sorting order (\code{sortID}) into account while aspect ratios are still acceptable.
#' @param sortID name of the variable that determines the order in which the rectangles are placed from top left to bottom right. Only applicable when \code{algorithm=="pivotSize"}. Also the values "size" and "color" can be used, which refer to \code{vSize} and \code{vColor} respectively. To inverse the sorting order, use "-" in the prefix. By default, large rectangles are placed top left.
#' @param mirror.x logical that determines whether the rectangles are mirrored horizontally
#' @param mirror.y logical that determines whether the rectangles are mirrored vertically
#' @param palette one of the following: 
#' \describe{
#'        \item{a color palette:}{i.e., a vector of hexadecimal colors (#RRGGBB)}
#'        \item{a name of a Brewer palette:}{See \code{RColorBrewer::display.brewer.all()} for the options. The palette can be reversed by prefixing with a "-". For treemap types "value" and "comp", a diverging palette should be chosen (default="RdYlGn"), for type "dens" a sequential (default="OrRd"). The default value for "depth" is "Set2".}
#'        \item{"HCL":}{Tree Colors are color schemes derived from the Hue-Chroma-Luminance color space model. This is only applicable for qualitative palettes, which are applied to the treemap types "index", "depth", and "categorical". For "index" and "categorical" this is the default value.}}
#' @param palette.HCL.options list of advanced options to obtain Tree Colors from  the HCL space (when \code{palette="HCL"}). This list contains: 
#' \describe{
#'        \item{\code{hue_start}:}{number between 0 and 360 that determines the starting hue value (default: 30)}
#'        \item{\code{hue_end}:}{number between \code{hue_start} and \code{hue_start + 360} that determines the ending hue value (default: 390)}
#'        \item{\code{hue_perm}:}{boolean that determines whether the colors are permuted such that adjacent levels get more distinguishable colors. If \code{FALSE}, then the colors are equally distributed from \code{hue_start} to \code{hue_end} (default: TRUE)}
#'        \item{\code{hue_rev}:}{boolean that determines whether the colors of even-numbered branched are reversed (to increase discrimination among branches)}
#'        \item{\code{hue_fraction}:}{number between 0 and 1 that determines the fraction of the hue circle that is used for recursive color picking: if 1 then the full hue circle is used, which means that the hue of the colors of lower-level nodes are spread maximally. If 0, then the hue of the colors of lower-level nodes are identical of the hue of their parents. (default: .5)}
#'        \item{\code{chroma}:}{chroma value of colors of the first-level nodes, that are determined by the first index variable (default: 60)}
#'        \item{\code{luminance}:}{luminance value of colors of the first-level nodes, i.e. determined by the first index variable (default: 70)}
#'        \item{\code{chroma_slope}:}{slope value for chroma of the non-first-level nodes. The chroma values for the second-level nodes are \code{chroma+chroma_slope}, for the third-level nodes \code{chroma+2*chroma_slope}, etc. (default: 5)}
#'        \item{\code{luminance_slope}:}{slope value for luminance of the non-first-level nodes (default: -10)}} For "depth" and "categorical" types, only the first two items are used. Use \code{\link{treecolors}} to experiment with these parameters.
#' @param range range of values (so vector of two) that correspond to the color legend. By default, the range of actual values, determined by \code{vColor}, is used. Only applicable for numeric types, i.e. "value", "comp", "dens", and "manual". Note that the range doesn't affect the colors in the treemap itself for "value" and "manual" types; this is controlled by \code{mapping}.
#' @param mapping vector of three values that specifies the mapping of the actual values, determined by \code{vColor}, to \code{palette}. The three values are respectively the minimum value, the mid value, and the maximum value. The mid value is particularly useful for diverging color palettes, where it defined the middle, neutral, color which is typically white or yellow. The \code{mapping} should cover the \code{range}. By default, for "value" treemaps, it is \code{c(-max(abs(values)), 0, max(abs(values)))}, where values are the actual values defined by \code{vColor}. For "manual" treemaps, the default setting is \code{c(min(values), mean(range(values)), max(values))}. A vector of two can also be specified. In that case, the mid value will be the average of those.  Only applicable for "value" and "manual" type treemaps.
#' @param n preferred number of categories by which numeric variables are discretized.
#' @param na.rm ignore missing vlues for the vSize variable (by default TRUE)
#' @param na.color color for missing values for the vColor variable
#' @param na.text legend label for missing values for the vColor variable
#' @param fontsize.title font size of the title
#' @param fontsize.labels font size(s) of the data labels, which is either a single number that specifies the font size for all aggregation levels, or a vector that specifies the font size for each aggregation level. Use value \code{0} to omit the labels for the corresponding aggregation level. 
#' @param fontsize.legend font size for the legend
#' @param fontcolor.labels Specifies the label colors. Either a single color value, or a vector of color values one for each aggregation level. By default, white and black colors are used, depending on the background (\code{bg.labels}).
#' @param fontface.labels either a single value, or a vector of values one for each aggregation level. Values can be integers  If an integer, following the R base graphics standard: 1 = plain, 2 = bold, 3 = italic, 4 = bold italic, or characters: \code{"plain"}, \code{"bold"}, \code{"italic"}, \code{"oblique"}, and \code{"bold.italic"}.
#' @param fontfamily.title font family of the title. Standard values are "serif", "sans", "mono", "symbol". Mapping is device dependent. 
#' @param fontfamily.labels font family of the labels in each rectangle. Standard values are "serif", "sans", "mono", "symbol". Mapping is device dependent. 
#' @param fontfamily.legend font family of the legend. Standard values are "serif", "sans", "mono", "symbol". Mapping is device dependent. 
#' @param border.col color of borders drawn around each rectangle. Either one color for all rectangles or a vector of colors, or one for each aggregation level
#' @param border.lwds thicknesses of border lines. Either one number specifies the line thicknesses (widths) for all rectangles or a vector of line thicknesses for each aggregation level.
#' @param lowerbound.cex.labels multiplier between 0 and 1 that sets the lowerbound for the data label font sizes: 0 means draw all data labels, and 1 means only draw data labels if they fit (given \code{fontsize.labels}).
#' @param inflate.labels logical that determines whether data labels are inflated inside the rectangles. If \code{TRUE}, \code{fontsize.labels} does not determine the fontsize anymore, but it still determines the minimum fontsize in combination with  \code{lowerbound.cex.labels}.
#' @param bg.labels background color of high aggregation labels. Either a color, or a number between 0 and 255 that determines the transparency of the labels. In the latter case, the color itself is determined by the color of the underlying rectangle. For "value" and "categorical" treemaps, the default is (slightly) transparent grey (\code{"#CCCCCCDC"}), and for the other types slightly transparent: \code{220}.
#' @param force.print.labels logical that determines whether data labels are being forced to be printed if they don't fit.
#' @param overlap.labels number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1 means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5 times their area size.
#' @param align.labels object that specifies the alignment of the labels. Either a character vector of two values specifying the horizontal alignment (\code{"left"}, \code{"center"}, or \code{"right"}) and the vertical alignment (\code{"top"}, \code{"center"}, or \code{"bottom"}), or a list of sush character vectors, one for each aggregation level.
#' @param xmod.labels the horizontal position modification of the labels in inches. Options: a single value, a vector or a list that specifies the modification for each aggregation level. If a list is provided, each list item consists of a single value or a named vector that specify the modification per label.
#' @param ymod.labels the vertical position modification of the labels in inches. Options: a single value, a vector or a list that specifies the modification for each aggregation level. If a list is provided, each list item consists of a single value or a named vector that specify the modification per label.
#' @param eval.labels should the text labels, i.e. the factor labels of the \code{index} variables, be evaluated as expressions? Useful for printing mathematical symbols or equations.
#' @param position.legend position of the legend: \code{"bottom"}, \code{"right"}, or \code{"none"}. For "categorical" and "index" treemaps, \code{"right"} is the default value, for "index" treemap, \code{"none"}, and for the other types, \code{"bottom"}.
#' @param reverse.legend should the legend be reversed?
#' @param format.legend a list of additional arguments for the formatting of numbers in the legend to pass to \code{format()}; only applies if \code{type} is \code{"value"}, \code{"dens"} or \code{"manual"}.
#' @param drop.unused.levels logical that determines whether unused levels (if any) are shown in the legend. Applicable for "categorical" treemap type.
#' @param aspRatio preferred aspect ratio of the main rectangle, defined by width/height. When set to \code{NA}, the available window size is used.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw in. By default it is not specified, which means that a new plot is created. Useful when drawing small multiples, or when placing a treemap in a custom grid based plot.
#' @param draw logical that determines whether to draw the treemap.
#' @param ... arguments to be passed to other functions. Currently, only \code{fun.aggregate} takes optional arguments. 
#' @return A list is silently returned:
#'	\item{tm}{a \code{data.frame} containing information about the rectangles: indices, sizes, original color values, derived color values, depth level, position (x0, y0, w, h), and color.}
#'  \item{type}{argument type}
#'  \item{vSize}{argument vSize}
#'  \item{vColor}{argument vColor}
#'  \item{stdErr}{standard errors}
#'  \item{algorithm}{argument algorithm}
#'  \item{vpCoorX}{x-coordinates of the treemap within the whole plot}
#'  \item{vpCoorY}{y-coordinates of the treemap within the whole plot}
#'  \item{aspRatio}{aspect ratio of the treemap}
#'  \item{range}{range of the color values scale}
#' @references
#' Bederson, B., Shneiderman, B., Wattenberg, M. (2002) Ordered and Quantum Treemaps: Making Effective Use of 2D Space to Display Hierarchies. ACM Transactions on Graphics, 21(4): 833-854.
#'
#' Bruls, D.M., C. Huizing, J.J. van Wijk. Squarified Treemaps. In: W. de Leeuw, R. van Liere (eds.), Data Visualization 2000, Proceedings of the joint Eurographics and IEEE TCVG Symposium on Visualization, 2000, Springer, Vienna, p. 33-42.
#' 
#' @example ../examples/treemap.R
#' @import data.table
#' @import RColorBrewer
#' @import grid
#' @import colorspace
#' @importFrom grDevices col2rgb colorRampPalette hcl rgb
#' @importFrom graphics par
#' @importFrom methods as
#' @importFrom stats rlnorm rnorm rpois
#' @export
treemap <-
    function(dtf, 
             index, 
             vSize, 
             vColor=NULL, 
             stdErr=NULL,
             type="index",
             fun.aggregate="sum",
             title=NA,
             title.legend=NA,
             algorithm="pivotSize",
             sortID="-size",
             mirror.x=FALSE,
             mirror.y=FALSE,
             palette=NA,
             palette.HCL.options=NULL,
             range=NA,
             mapping=NA,
             n=7,
             na.rm = TRUE,
             na.color = "#DDDDDD",
             na.text = "Missing",
             fontsize.title=14, 
             fontsize.labels=11, 
             fontsize.legend=12,
             fontcolor.labels=NULL,
             fontface.labels=c("bold", rep("plain", length(index)-1)),
             fontfamily.title="sans",
             fontfamily.labels="sans",
             fontfamily.legend="sans",
             border.col="black",
             border.lwds=c(length(index)+1, (length(index)-1):1),
             lowerbound.cex.labels=0.4,
             inflate.labels=FALSE,
             bg.labels= NULL,
             force.print.labels=FALSE,
             overlap.labels=0.5,
             align.labels = c("center", "center"),
             xmod.labels = 0,
             ymod.labels = 0,
             eval.labels = FALSE,
             position.legend=NULL,
             reverse.legend=FALSE,
             format.legend=NULL,
             drop.unused.levels = TRUE,
             aspRatio=NA,
             vp=NULL,
             draw=TRUE,
             ...) {
        s <- NULL #for CMD check
        
        vColor.temp <- i <- w <- h <- NULL
        
        #require(data.table)
        #############
        ## Preprocess arguments
        #############
        
        ## required arguments
        if (!exists("dtf")) stop("Dataframe <dtf> not defined")
        if (!exists("index")) stop("Attribute <index> not defined")
        if (!exists("vSize")) stop("Attribute <vSize> not defined")
        
        ## check and preprocess arguments one by one
        
        # dtf
        if (!inherits(dtf, "data.frame")) stop("Object <dtf> is not a data.frame")
        
        if (nrow(dtf)==0) stop("data.frame doesn't have any rows")
        
        # index
        if (any(!index %in% names(dtf))) stop("<index> contains invalid column names")
        
        depth <- length(index)
        
        # vSize
        if (length(vSize)!=1) stop("vSize should be one column name")
        if (!vSize %in% names(dtf)) stop("vSize is invalid column name")
        if (!is.numeric(dtf[[vSize]]))
            stop(paste("Column(s) in vSize not numeric",sep=""))
        
        #stdErr
        if (!is.null(stdErr)) {
            if (length(stdErr)!=1) stop("stdErr should be one column name")
            if (!stdErr %in% names(dtf)) stop("stdErr is invalid column name")
            if (!is.numeric(dtf[[stdErr]]))
                stop(paste("Column(s) in stdErr not numeric",sep=""))
        }
        else {
            stdErr <- vSize
        }
        
        # vColor
        vColorMplySplit <- function(vColor) {
            divided <- 0
            vColorMply <- unlist(strsplit(vColor, split="*", fixed=TRUE))
            if (length(vColorMply)==1) {
                vColorMply <- unlist(strsplit(vColor, split="/", fixed=TRUE))
                if (length(vColorMply)==1) {
                    vColorMply <- c(vColorMply, 1)
                } else {
                    vColorMply[2] <- (1/as.numeric(vColorMply[2]))
                    divided <- 1
                }
            }
            return (c(vColorMply, divided))
        }
        
        if (type %in% c("index", "depth")) vColor <- NULL
        if (!is.null(vColor)) {
            if (length(vColor)!=1) stop("length of vColor should be one")
            vColor2 <- vColorMplySplit(vColor)
            vColorX <- as.numeric(vColor2[2])
            if (is.na(vColorX)) stop("vColor: invalid scale factor")
            vColor <- vColor2[1]
            if (!(vColor %in% names(dtf)))
                stop("Invalid column name in vColor.")
            vColorDiv <- as.logical(as.numeric(vColor2[3]))
        }	
        
        # type
        if (!type %in% c("value", "categorical", "comp", "dens", "index", "depth", "color", "manual")) 
            stop("Invalid type")
        
        # fun.aggregate
        if (!is.function(match.fun(fun.aggregate)))
            stop("fun.aggregate is not a function")
        
        if (type=="dens") fun.aggregate <- "weighted.mean"
        
        # title	
        if (!is.na(title[1]) && length(title) != 1) {
            warning("Length of title should be 1")
            title <- NA}
        if (is.na(title[1])) {	
            title <- vSize
        }
        
        # title.legend	
        if (!is.na(title.legend[1]) && length(title.legend) != 1) {
            warning("Length of title.legend should be 1")
            title.legend <- NA}
        
        formatColorTitle <- function(var, varX=NA, var2=NA, var2X=NA, div) {
            if (!is.na(var2)) {
                # density treemap
                if (var2X!=1) {
                    if (div)
                        var2 <-paste(1/var2X, var2, sep="*")
                    else
                        var <- paste(var2X, var, sep="*")
                }
                var <- paste(var,"per",var2,sep=" ")
            } else {
                # other treemap types
                if (varX!=1) {
                    if (div)
                        var <-paste(var, 1/varX, sep="/")
                    else 
                        var <-paste(varX, var, sep="*")
                }
            }
            var
        }
        
        if (is.na(title.legend[1])) {	
            suppressWarnings({
                if (!is.null(vColor)) {
                    if (type=="dens") 
                        title.legend <- formatColorTitle(var=vColor, var2=vSize, 
                                                         var2X=vColorX, div=vColorDiv)
                    else
                        title.legend <- formatColorTitle(var=vColor, varX=vColorX, 
                                                         div=vColorDiv)
                } else title.legend <- ""
            })
        }
        
        # algorithm
        if (!algorithm %in% c("pivotSize", "squarified")) 
            stop("Invalid algorithm")
        
        # sortID
        if (length(sortID)!=1)
            stop("sortID should be of length one")
        ascending <- substr(sortID,1,1)!="-"
        if (!ascending) sortID <- substr(sortID,2,nchar(sortID))
        
        if (sortID=="size") sortID <- vSize
        if (sortID=="color") sortID <- vColor
        if (sortID=="se") sortID <- stdErr
        
        if (!(sortID %in% names(dtf)))
            stop("Incorrect sortID")
        
        
        # palette
        if (is.na(palette[1])) {
            if (type == "comp") {
                palette <- brewer.pal(11,"RdYlGn")
            } else if (type == "dens") {
                palette <- brewer.pal(9,"OrRd")
            } else if (type == "depth") {
                palette <- brewer.pal(8,"Set2")
            } else if (type == "index") {
                palette <- "HCL"
            } else if (type == "value") {
                palette <- brewer.pal(11,"RdYlGn")
            } else if (type == "categorical") {
                palette <- "HCL"
            } else if (type == "manual") {
                stop("For \"manual\" treemaps, a palette should be provided.")
            }
        } else {
            reverse <- (substr(palette[1], 1, 1)=="-")
            if (reverse) palette[1] <- substr(palette[1], 2, nchar(palette[1]))
            if ((length(palette)==1) && (palette[1] %in%row.names(brewer.pal.info))) {
                # brewer palettes
                palette <- brewer.pal(brewer.pal.info[palette, "maxcolors"], palette)
                if (reverse) palette <- rev(palette)
            } else {
                if (palette[1]=="HCL" && !(type %in% c("depth", "index", "categorical"))) {
                    stop("HCL palette only applicable for treemap types \"depth\", \"index\" and \"categorical\".")
                }
                if (palette[1]!="HCL" & inherits(try(col2rgb(palette), silent=TRUE), "try-error")) {
                    stop("color palette is not correct")
                }
            }
        }
        
        palette.HCL.options <- tmSetHCLoptions(palette.HCL.options)
        #         # palette.HCL.options
        #         palette.HCL.options.temp <- list(hue_start=30, hue_end=390, hue_spread=TRUE, hue_fraction=0.5, chroma=60, luminance=70, chroma_slope=5, luminance_slope=-10)
        #         if (!missing(palette.HCL.options)) {
        #             if (!is.list(palette.HCL.options) | !all(names(palette.HCL.options)%in%names(palette.HCL.options.temp))) stop("Incorrect palette.HCL.options")
        #             palette.HCL.options.temp[names(palette.HCL.options)] <- palette.HCL.options
        #         }
        #         palette.HCL.options <- palette.HCL.options.temp
        
        # range
        if (!all(is.na(range))) {
            if (length(range)!=2)
                stop("length range should be 2")
            if (!is.numeric(range))
                stop("range is not numeric")
        } else range <- c(NA, NA)
        
        # mapping
        if (!all(is.na(mapping))) {
            if (!length(mapping) %in% c(2,3))
                stop("length range should be 2 or 3")
            if (!is.numeric(mapping))
                stop("range is not numeric")
            if (length(mapping)==2) {
                mapping <- c(mapping[1], mean(mapping), mapping[2])
            }
        } else mapping <- c(NA, NA, NA)
        
        # fontsize.title
        if (length(fontsize.title)!=1 || 
            !is.numeric(fontsize.title))
            stop("Invalid fontsize.title")
        if (title=="") fontsize.title <- 0
        
        # fontsize.labels
        if (!is.numeric(fontsize.labels))
            stop("Invalid fontsize.labels")
        fontsize.labels <- rep(fontsize.labels, length.out=depth)
        #cex_indices <- fontsize.labels / max(min(fontsize.labels), 1)
        cex_indices <- fontsize.labels / 12 #max(fontsize.labels)
        
        # fontsize.legend
        if (length(fontsize.legend)!=1 || 
            !is.numeric(fontsize.legend))
            stop("Invalid fontsize.legend")
        
        
        # fontcolor.labels
        if (!missing(fontcolor.labels)) if (length(fontcolor.labels)!=depth) fontcolor.labels <- rep(fontcolor.labels, length.out=depth)
        
        # fontface.labels
        if (length(fontface.labels)!=depth) fontface.labels <- rep(fontface.labels, length.out=depth)
        
        # fontfamily
        
        
        # border.col and border.lwds
        if (length(border.col)!=depth) border.col <- rep(border.col, length.out=depth)
        if (length(border.lwds)!=depth) border.lwds <- rep(border.lwds, length.out=depth)
        
        # lowerbound.cex.labels
        if (length(lowerbound.cex.labels)!=1 ||
            !is.numeric(lowerbound.cex.labels))
            stop("Invalid lowerbound.cex.labels")
        if (lowerbound.cex.labels < 0 || lowerbound.cex.labels > 1)
            stop("lowerbound.cex.labels not between 0 and 1")
        
        # inflate.labels
        if (length(inflate.labels)!=1 ||
            class(inflate.labels) !="logical")
            stop("Invalid inflate.labels")
        
        # bg.labels
        if (missing(bg.labels)) {
            bg.labels <- ifelse(type %in% c("value", "categorical"), "#CCCCCCDC", 220)
        } else {
            if (length(bg.labels)!=1) stop("Invalid bg.labels")
            if (!is.numeric(bg.labels)) {
                if (class(try(col2rgb(bg.labels), silent=TRUE))=="try-error") stop("Invalid bg.labels")
            } else {
                if (bg.labels < 0 || bg.labels > 255) stop("bg.labels should be between 0 and 255")
            }
        }
        
        # force.print.labels
        if (length(force.print.labels)!=1 ||
            class(force.print.labels) !="logical")
            stop("Invalid force.print.labels")
        
        # overlap.labels
        if (length(overlap.labels)!=1 ||
            !is.numeric(overlap.labels))
            stop("Invalid overlap.labels")
        if (overlap.labels<0 || overlap.labels > 1) stop("overlap.labels should be between 0 and 1")
        
        #align.labels
        if (!is.list(align.labels)) align.labels <- list(align.labels)
        if (length(align.labels) !=depth) align.labels <- rep(align.labels, length.out=depth)
        lapply(align.labels, function(al) if (!(al[1]%in% c("left", "center", "centre", "right") && 
                                                al[2]%in% c("top", "center", "centre", "bottom"))) stop("incorrect align.labels"))
        
        #xmod.labels and ymod.labels
        if (is.list(xmod.labels)) xmod.labels <- as.list(xmod.labels)
        if (is.list(ymod.labels)) ymod.labels <- as.list(ymod.labels)
        
        if (length(xmod.labels)!=depth) xmod.labels <- rep(xmod.labels, length.out=depth)
        if (length(ymod.labels)!=depth) ymod.labels <- rep(ymod.labels, length.out=depth)
        
        # position.legend
        if (missing(position.legend)) {
            position.legend <- switch(type, categorical="right", depth="right", index="none", color="none", "bottom")
        } else {
            if (!position.legend %in% c("right", "bottom", "none")) 
                stop("Invalid position.legend")	
        }
        
        # drop.unused.levels
        if (length(drop.unused.levels)!=1 ||
            class(drop.unused.levels) !="logical")
            stop("Invalid drop.unused.levels")
        
        # aspRatio
        if (length(aspRatio)!=1 || (!is.na(aspRatio[1]) && !is.numeric(aspRatio)))
            stop("Invalid aspRatio")
        
        args <- list(...)
        args$na.rm <- na.rm
        
        ###########
        ## prepare data for aggregation
        ###########
        if (inherits(dtf, c("tbl_df"))) {
            dtfDT <- as.data.table(data.frame(dtf[, c(index, vSize, vColor, sortID, stdErr)]))
        } else if (is.data.table(dtf)) {
            dtfDT <- copy(dtf[, c(index, vSize, vColor, sortID, stdErr), with=FALSE])
        } else {
            dtfDT <- as.data.table(dtf[, c(index, vSize, vColor, sortID, stdErr)])
        }
        
        if (is.null(vColor)) {
            vColor <- "vColor.temp"
            vColorX <- 1
            dtfDT[, vColor.temp:=1]
            setcolorder(dtfDT, c(1:(ncol(dtfDT)-3), ncol(dtfDT), ncol(dtfDT)-2, ncol(dtfDT)-1))
        }
        
        indexList <- paste0("index", 1:depth)
        setnames(dtfDT, old=1:ncol(dtfDT), new=c(indexList, "s", "c", "i", "se"))
        
        if (vColorX!=1) dtfDT[, c:=c/vColorX]
        
        if (fun.aggregate=="weighted.mean") {
            if ("w" %in% names(args)) {
                if (is.character(args$w)) {
                    dtfDT[, w:=eval(parse(text=args$w))]
                } else {
                    dtfDT[, w:=args$w]
                }
            } else {
                dtfDT[, w:=s]
            }
        } else {
            dtfDT[, w:=1]
        }
        
        
        ## cast non-factor index columns to factor
        for (d in 1:depth) {
            if (is.numeric(dtfDT[[d]])) { 
                fact <- factor(dtfDT[[d]], levels=sort(unique(dtfDT[[d]])))
                dtfDT[, (d):=fact] 
            } else if (!is.factor(dtfDT[[d]])) {
                fact <- factor(dtfDT[[d]])
                dtfDT[, (d):=fact]
            }
        }
        
        
        ## cast character color columns to factor
        if (is.character(dtfDT[["c"]])) {
            dtfDT[, c:=factor(c)]
        }
        
        
        ## cast sortID to numeric
        if (!is.numeric(dtfDT[["i"]])) {
            warning("sortID must be a numeric variable")
            dtfDT[, i:=integer(nrow(dtfDT))]
        }
        
        ## cast se to numeric
        if (!is.null(stdErr) && !is.numeric(dtfDT[["se"]])) {
            warning("stdErr must be a numeric variable")
            dtfDT[, "se":=integer(dtfDT[["se"]])]
        }
        
        setkeyv(dtfDT, indexList)
        
        
        ###########
        ## process treemap
        ###########
        datlist <- tmAggregate(dtfDT, indexList, type, ascending, drop.unused.levels, fun.aggregate, args)
        catLabels <- switch(type, categorical=levels(datlist$c), index=levels(datlist$index1), depth=index, standErr=datlist$se, NA)
        
        if (!draw) position.legend <- "none"
        vps <- tmGetViewports(vp, fontsize.title, fontsize.labels, fontsize.legend,
                              position.legend, type, aspRatio, title.legend, catLabels)
        if (draw) tmPrintTitles(vps, title, title.legend, position.legend, fontfamily.title, fontfamily.legend)
        if (type == "color") {
            datlist$color <- as.character(datlist$c)
            datlist$colorvalue <- NA
        } else {
            attr(datlist, "range") <- 1:2
            datlist <- tmColorsLegend(datlist, vps, position.legend, type, palette, range, mapping, indexNames=index, palette.HCL.options=palette.HCL.options, border.col, fontfamily.legend, n, na.color, na.text, format.legend, reverse.legend)
        }
        datlist <- tmGenerateRect(datlist, vps, indexList, algorithm)
        
        if (mirror.x) datlist <- within(datlist, x0 <- 1 - x0 - w)
        if (mirror.y) datlist <- within(datlist, y0 <- 1 - y0 - h)
        
        if (draw) {
            tmDrawRect(datlist, vps, indexList, lowerbound.cex.labels, inflate.labels, bg.labels, 
                       force.print.labels, cex_indices, overlap.labels, border.col, border.lwds, 
                       fontcolor.labels, fontface.labels, fontfamily.labels, align.labels, xmod.labels, ymod.labels, eval.labels)
        }
        
        upViewport(0 + !is.null(vp))
        
        # return treemap info
        tm <- datlist[, c(indexList, "s", "c", "se", "colorvalue", "l", "x0", "y0", "w", "h", "color"), with=FALSE]
        
        # recover original color values from densities
        if (type=="dens") tm[,c:=c*s]
        
        setnames(tm, c(index, "vSize", "vColor", "stdErr", "vColorValue", "level", "x0", "y0", "w", "h", "color"))
        
        tmSave <- list(tm = as.data.frame(tm),
                       type = type,
                       vSize = vSize,
                       vColor = ifelse(vColor=="vColor.temp", NA, vColor),
                       stdErr = stdErr,
                       algorithm = algorithm,
                       vpCoorX = vps$vpCoorX,
                       vpCoorY = vps$vpCoorY,
                       aspRatio = vps$aspRatio,
                       range = range,
                       mapping = mapping,
                       draw = draw)
        invisible(tmSave)
    }

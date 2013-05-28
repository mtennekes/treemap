#' Treemap
#'
#' A treemap is a space-filling visualization of hierarchical structures. This function offers great flexibility to draw treemaps. Required is a data.frame (\code{dtf}) that contains one or more hierarchical index columns given by \code{index}, a column that determines the rectangle area sizes (\code{vSize}), and optionally a column that determines the rectangle colors (\code{vColor}). The way how rectangles are colored is determined by the argument \code{type}.
#' 
#' @param dtf a data.frame. Required.
#' @param index    vector of column names in \code{dtf} that specify the aggregation indices. It could contain only one column name, which results in a treemap without hierarchy. If multiple column names are provided, the first name is the highest aggregation level, the second name the second-highest aggregation level, and so on. Required. 
#' @param vSize name of the column in \code{dtf} that specifies the sizes of the rectangles. Required.
#' @param vColor name of the column that, in combination with \code{type}, determines the colors of the rectangles. The variable can be scaled by the addition of "*<scale factor>" or "/<scale factor>". 
#' @param type type of the treemap, which determines how the rectangles are colored:
#' \describe{
#'    	\item{\code{"index"}:}{colors are determined by the \code{index} variables. Different branches in the hierarchical tree get different colors. For this type, \code{vColor} is not needed.}
#'    	\item{\code{"value"}:}{the numeric \code{vColor}-column is directly mapped to a color palette.}
#'		\item{\code{"comp"}:}{colors indicate change of the \code{vSize}-column with respect to the numeric \code{vColor}-column in percentages. Note: the negative scale may be different from the positive scale in order to compensate for the ratio distribution.}
#'		\item{\code{"dens"}:}{colors indicate density. This is analogous to a population density map where \code{vSize}-values are area sizes, \code{vColor}-values are populations per area, and colors are computed as densities (i.e. population per squared km).}
#'		\item{\code{"depth"}:}{each aggregation level (defined by \code{index}) has a distinct color.}
#'    	\item{\code{"categorical"}:}{\code{vColor} is a factor column that determines the color.}
#'      \item{\code{"color"}:}{\code{vColor} is a vector of colors in the hexadecimal (#RRGGBB) format}}
#' @param title title of the treemap.
#' @param title.legend title of the legend.
#' @param algorithm name of the used algorithm: \code{"squarified"} or \code{"pivotSize"}. The squarified treemap algorithm (Bruls et al., 2000) produces good aspect ratios, but ignores the sorting order of the rectangles (\code{sortID}). The ordered treemap, pivot-by-size, algorithm (Bederson et al., 2002) takes the sorting order (\code{sortID}) into account while aspect ratios are still acceptable.
#' @param sortID name of the variable that determines the order in which the rectangles are placed from top left to bottom right. Only applicable when \code{algorithm=="pivotSize"}. Also the values "size" and "color" can be used, which refer to \code{vSize} and \code{vColor} respectively. To inverse the sorting order, use "-" in the prefix. By default, large rectangles are placed top left.
#' @param palette one of the following: 
#' \describe{
#'        \item{a color palette:}{i.e., a vector of hexadecimal colors (#RRGGBB)}
#'        \item{a name of a Brewer palette:}{See \code{RColorBrewer::display.brewer.all()} for the options. The palette can be reversed by prefixing with a "-". For treemap types "value" and "comp", a diverging palette should be chosen (default="RdYlGn"), for type "dens" a sequential (default="OrRd"). The default value for "depth" is "Set2".}
#'        \item{"HCL":}{Colors are derived from the Hue-Chroma-Luminance color space model. This is only applicable for qualitative palettes, which are applied to the treemap types "index", "depth", and "categorical". For "index" and "categorical" this is the default value.}}
#' @param palette.HCL.options list of advanced options to pick colors from  the HCL space (when \code{palette="HCL"}). This list contains: 
#' \describe{
#'        \item{\code{hue_start}:}{number between 0 and 360 that determines the starting hue value (default: 30)}
#'        \item{\code{hue_end}:}{number between \code{hue_start} and \code{hue_start + 360} that determines the ending hue value (default: 390)}
#'        \item{\code{hue_spread}:}{boolean that determines whether the colors are spread such that adjacent levels get more distinguishable colors. If \code{FALSE}, then the colors are equally distributed from \code{hue_start} to \code{hue_end} (default: TRUE)}
#'        \item{\code{hue_fraction}:}{number between 0 and 1 that determines the fraction of the hue circle that is used for recursive color picking: if 0 then the full hue circle is used, which means that the hue of the colors of lower-level nodes are spread maximally. If 1, then the hue of the colors of lower-level nodes are identical of the hue of their parents. (default: .5)}
#'        \item{\code{chroma}:}{chroma value of colors of the first-level nodes, that are determined by the first index variable (default: 60)}
#'        \item{\code{luminance}:}{luminance value of colors of the first-level nodes, i.e. determined by the first index variable (default: 70)}
#'        \item{\code{chroma_slope}:}{slope value for chroma of the non-first-level nodes. The chroma values for the second-level nodes are \code{chroma+chroma_slope}, for the third-level nodes \code{chroma+2*chroma_slope}, etc. (default: 5)}
#'        \item{\code{luminance_slope}:}{slope value for luminance of the non-first-level nodes (default: -10)}} For "depth" and "categorical" types, only the first two items are used.
#' @param range range of values that determine the colors. Only applicable for types "value", "comp", and "dens". When omitted, the range of actual values is used. This range is mapped to \code{palette}.
#' @param fontsize.title (maximum) font size of the title
#' @param fontsize.labels font size(s) of the data labels, which can be:
#' \itemize{
#' \item one number, which specifies the font size for all aggregation levels
#' \item vector of two numbers, which specific the font sizes for 1) the highest and 2) the other aggregation levels
#' \item vector of three numbers, which specific the font sizes for 1) the highest, 2) any in-between, and 3) the lowest aggregation level.}
#' @param fontsize.legend (maximum) font size of the legend
#' @param lowerbound.cex.labels multiplier between 0 and 1 that sets the lowerbound for the data label font sizes: 0 means draw all data labels, and 1 means only draw data labels if they fit (given \code{fontsize.labels}).
#' @param inflate.labels logical that determines whether data labels are inflated inside the rectangles. If TRUE, \code{fontsize.labels} is does not determine the maximum fontsize, but it does determine in combination with  \code{lowerbound.cex.labels} the minimum fontsize.
#' @param bg.labels background color of high aggregation labels. Either a color, or a number between 0 and 255 that determines the transparency of the labels. In the latter case, the color itself is determined by the color of the underlying rectangle. For "value" and "categorical" treemaps, the default is (slightly) transparent grey (\code{"#CCCCCCDC"}), and for the other types slightly transparent: \code{220}.
#' @param force.print.labels logical that determines whether data labels are being forced to be printed if they don't fit.
#' @param overlap.labels number between 0 and 1 that determines the tolerance of the overlap between labels. 0 means that labels of lower levels are not printed if higher level labels overlap, 1 means that labels are always printed. In-between values, for instance the default value .5, means that lower level labels are printed if other labels do not overlap with more than .5 times their area size.
#' @param position.legend position of the legend: \code{"bottom"}, \code{"right"}, or \code{"none"}. For "categorical" and "index" treemaps, \code{"right"} is the default value, for "index" treemap, \code{"none"}, and for the other types, \code{"bottom"}.
#' @param drop.unused.levels logical that determines whether unused levels (if any) are shown in the legend. Applicable for "categorical" treemap type.
#' @param aspRatio preferred aspect ratio of the main rectangle, defined by width/height. When set to \code{NA}, the available window size is used.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw in. By default it is not specified, which means that a new plot is created. Useful when drawing small multiples, or when placing a treemap in a custom grid based plot.
#' @return A list is silently returned:
#'	\item{tm}{a \code{data.frame} containing information about the rectangles}
#'  \item{vSize}{argument vSize}
#'  \item{vColor}{argument vColor}
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
#' @export
treemap <-
    function(dtf, 
             index, 
             vSize, 
             vColor=NULL, 
             type="index",
             title=NA,
             title.legend=NA,
             algorithm="pivotSize",
             sortID="-size",
             palette=NA,
             palette.HCL.options=NULL,
             range=NA,
             fontsize.title=14, 
             fontsize.labels=11, 
             fontsize.legend=12,
             lowerbound.cex.labels=0.4,
             inflate.labels=FALSE,
             bg.labels= ifelse(type %in% c("value", "categorical"), "#CCCCCCDC", 220),
             force.print.labels=FALSE,
             overlap.labels=0.5,
             position.legend=switch(type, categorical="right", depth="right", index="none", "bottom"),
             drop.unused.levels = TRUE,
             aspRatio=NA,
             vp=NULL) {
        
        vColor.temp <- i <- NULL
        
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
        
        # index
        if (any(!index %in% names(dtf))) stop("<index> contains invalid column names")
        
        # vSize
        if (length(vSize)!=1) stop("vSize should be one column name")
        if (!vSize %in% names(dtf)) stop("vSize is invalid column name")
        if (!is.numeric(dtf[[vSize]]))
            stop(paste("Column(s) in vSize not numeric",sep=""))
        
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
        if (!type %in% c("value", "categorical", "comp", "dens", "index", "depth", "color")) 
            stop("Invalid type")
        
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
            options(warn=-1) 
            if (!is.null(vColor)) {
                if (type=="dens") 
                    title.legend <- formatColorTitle(var=vColor, var2=vSize, 
                                                     var2X=vColorX, div=vColorDiv)
                else
                    title.legend <- formatColorTitle(var=vColor, varX=vColorX, 
                                                     div=vColorDiv)
            } else title.legend <- ""
            options(warn=0) 
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
                if (palette[1]!="HCL") if (class(try(col2rgb(palette), silent=TRUE))=="try-error") 
                    stop("color palette is not correct")
            }
        }
        
        # palette.HCL.options
        palette.HCL.options.temp <- list(hue_start=30, hue_end=390, hue_spread=TRUE, hue_fraction=0.5, chroma=60, luminance=70, chroma_slope=5, luminance_slope=-10)
        if (!missing(palette.HCL.options)) {
            if (!is.list(palette.HCL.options) | !all(names(palette.HCL.options)%in%names(palette.HCL.options.temp))) stop("Incorrect palette.HCL.options")
            palette.HCL.options.temp[names(palette.HCL.options)] <- palette.HCL.options
        }
        palette.HCL.options <- palette.HCL.options.temp
        
        # range
        if (!any(is.na(range))) {
            if (length(range)!=2)
                stop("length range is not 2")
            if (!is.numeric(range))
                stop("range is not numeric")
        }
        
        # fontsize.title
        if (length(fontsize.title)!=1 || 
                !is.numeric(fontsize.title))
            stop("Invalid fontsize.title")
        
        # fontsize.title
        if (length(fontsize.title)!=1 || 
                !is.numeric(fontsize.title))
            stop("Invalid fontsize.title")
        
        # fontsize.labels
        if (!length(fontsize.labels) %in% 1:3 || 
                !is.numeric(fontsize.labels))
            stop("Invalid fontsize.labels")
        if (length(fontsize.labels)==1)
            fontsize.labels <- rep(fontsize.labels, 3)
        else if (length(fontsize.labels)==2)
            fontsize.labels <- fontsize.labels[c(1,2,2)]
        cex_indices <- fontsize.labels / fontsize.labels[1]
        
        # fontsize.legend
        if (length(fontsize.legend)!=1 || 
                !is.numeric(fontsize.legend))
            stop("Invalid fontsize.legend")
        
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
        if (length(bg.labels)!=1) stop("Invalid bg.labels")
        if (!is.numeric(bg.labels)) {
            if (class(try(col2rgb(bg.labels), silent=TRUE))=="try-error") stop("Invalid bg.labels")
        } else {
            if (bg.labels < 0 || bg.labels > 255) stop("bg.labels should be between 0 and 255")
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

        # position.legend
        if (!position.legend %in% c("right", "bottom", "none")) 
            stop("Invalid position.legend")	
        
        # drop.unused.levels
        if (length(drop.unused.levels)!=1 ||
                class(drop.unused.levels) !="logical")
            stop("Invalid drop.unused.levels")

        # aspRatio
        if (length(aspRatio)!=1 || (!is.na(aspRatio[1]) && !is.numeric(aspRatio)))
            stop("Invalid aspRatio")
        
        
        ###########
        ## prepare data for aggregation
        ###########
        
        if (is.data.table(dtf)) {
            dtfDT <- copy(dtf[, c(index, vSize, vColor, sortID)])
        } else {
            dtfDT <- as.data.table(dtf[, c(index, vSize, vColor, sortID)])
        }
        
        if (is.null(vColor)) {
            vColor <- "vColor.temp"
            vColorX <- 1
            dtfDT[, vColor.temp:=1]
            setcolorder(dtfDT, c(1:(ncol(dtfDT)-2), ncol(dtfDT), ncol(dtfDT)-1))
        }
        
        depth <- length(index)
        indexList <- paste0("index", 1:depth)
        setnames(dtfDT, old=names(dtfDT), new=c(indexList, "s", "c", "i"))
        
        if (vColorX!=1) dtfDT[, c:=c/vColorX]
        ## cast non-factor index columns to factor
        for (d in 1:depth) {
            if (is.numeric(dtfDT[[d]])) { 
                fact <- factor(dtfDT[[d]], levels=sort(unique(dtfDT[[d]])))
                dtfDT[, d:=fact, with=FALSE] 
            } else if (!is.factor(dtfDT[[d]])) {
                fact <- factor(dtfDT[[d]])
                dtfDT[, d:=fact, with=FALSE]
            }
        }
        
        
        ## cast character color columns to factor
        if (is.character(dtfDT[["c"]])) {
            fact <- factor(dtfDT[["c"]])
            dtfDT[, "c":=fact, with=FALSE] 
        }
        
        
        ## cast sortID to numeric
        if (!is.numeric(dtfDT[["i"]])) {
            warning("sortID must be a numeric variable")
            dtfDT[, i:=integer(nrow(dtfDT))]
        }
        
        setkeyv(dtfDT, indexList)
        
        ###########
        ## process treemap
        ###########
        datlist <- tmAggregate(dtfDT, indexList, type, ascending, drop.unused.levels)
        catLabels <- switch(type, categorical=levels(datlist$c), index=levels(datlist$index1), depth=index, NA)
        vps <- tmGetViewports(vp, fontsize.title, fontsize.labels, fontsize.legend,
                              position.legend, type, aspRatio, title.legend, catLabels)
        tmPrintTitles(vps, title, title.legend, position.legend)
        if (type == "color") {
            datlist$color <- as.character(datlist$c)
        } else {
            datlist <- tmColorsLegend(datlist, vps, position.legend, type, palette, range, indexNames=index, palette.HCL.options=palette.HCL.options)
        }
        datlist <- tmGenerateRect(datlist, vps, indexList, algorithm)
        
        tmDrawRect(datlist, vps, indexList, lowerbound.cex.labels, inflate.labels, bg.labels, force.print.labels, cex_indices, overlap.labels)
        
        upViewport(0 + !is.null(vp))
        
        # return treemap info
        tm <- datlist[, c(indexList, "s", "l", "x0", "y0", "w", "h"), with=FALSE]
        setnames(tm, c(index, "size", "level", "x0", "y0", "w", "h"))
        
        tmSave <- list(tm = as.data.table(tm),
                       type = type,
                       vSize = vSize,
                       vColor = ifelse(vColor=="vColor.temp", NA, vColor),
                       algorithm = algorithm)
        invisible(tmSave)
    }

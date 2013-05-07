#' Create treemap
#'
#' Create treemap
#'
#' @param dtf a data.frame. Required.
#' @param index	vector containing the column names in \code{dtf} that contain the aggregation indices. Required.
#' @param vSize name of the variable that determines the sizes of the rectangles.  Required.
#' @param vColor name of the variable that, in combination with \code{type}, determines the colors of the rectangles. The variable can be scaled by the addition of "*<scale factor>" or "/<scale factor>". 
#' @param type the type of the treemap:
#' \describe{
#'    	\item{\code{"value"}:}{the \code{vColor}-variable is directly mapped to a color palette (by default Brewer's diverging color palette "RdBu").}
#'		\item{\code{"comp"}:}{colors indicate change of the \code{vSize}-variable with respect to the \code{vColor}-variable in percentages. Note: the negative scale may be different from the positive scale in order to compensate for the ratio distribution.}
#'		\item{\code{"dens"}:}{colors indicate density. This is aanalogous to a population density map where \code{vSize}-values are area sizes, \code{vColor}-values are populations per area, and colors are computed as densities (i.e.\ population per squared km's).}
#'		\item{\code{"depth"}:}{each aggregation level (defined by \code{index}) has a distinct color}
#'		\item{\code{"index"}:}{colors are determined by the \code{index} variables. Each aggregation of the first index variable is assigned to a color of \code{palette}. Each aggregation of on of the other index variables is assigned to a similar color, that varies on hue or lightness.}
#'    	\item{\code{"categorical"}:}{\code{vColor} is a categorical variable that determines the color}
#'      \item{\code{"color"}:}{\code{vColor} is a vector of colors in the hexadecimal (#RRGGBB) format}}
#' @param title title of the treemap.
#' @param subtitle subtitle of the treemap.
#' @param algorithm name of the used algorithm: \code{"squarified"} or \code{"pivotSize"}. The squarified treemap algorithm (Bruls et al., 2000) produces good aspect ratios, but ignores the sorting order of the rectangles (\code{sortID}). The ordered treemap, pivot-by-size, algorithm (Bederson et al., 2002) takes the sorting order (\code{sortID}) into account while aspect ratios are still acceptable.
#' @param sortID name of the variable that determines the order in which the rectangles are placed from top left to bottom right. Also the values "size" and "color" can be used. To inverse the sorting order, use "-" in the prefix. By default, large rectangles are placed top left. Only applicable when \code{algortihm=="pivotSize"}.
#' @param palette either a color palette or a name of a Brewer palette (see \code{display.brewer.all()}). A Brewer palette can be reversed by prefixing its name with a "-".
#' @param range range of values that determine the colors. When omitted, the range of actual values is used. This range is mapped to \code{palette}.
#' @param vColorRange deprecated, use \code{range} instead.
#' @param fontsize.title (maximum) font size of the title
#' @param fontsize.labels font size(s) of the data labels, which can be:
#' \itemize{
#' \item one number, which specifies the font size for all aggregation levels
#' \item vector of two numbers, which specific the font sizes for 1) the highest and 2) the other aggregation levels
#' \item vector of three numbers, which specific the font sizes for 1) the highest, 2) any in-between, and 3) the lowest aggregation level.}
#' @param fontsize.legend (maximum) font size of the legend
#' @param lowerbound.cex.labels multiplier between 0 and 1 that sets the lowerbound for the data label font sizes: 0 means draw all data labels, and 1 means only draw data labels if they fit at \code{fontsize.data}.
#' @param inflate.labels logical that determines whether data labels are inflated inside the rectangles.
#' @param bg.labels background color of labels of high aggregation levels. If set to \code{NA}, the color is determined by the color of the underlying rectangle. For value, categorical and linked treemaps, the default is transparent grey (\code{"#CCCCCCAA"}), and for the other types, \code{NA}.
#' @param force.print.labels logical that determines whether data labels are being forced to be printed (also when they don't fit).
#' @param position.legend position of the legend: \code{"bottom"}, \code{"right"}, or \code{"none"}. For categorical and index treemaps, \code{"right"} is the default value, for linked treemap, \code{"none"}, and for the other types, \code{"bottom"}.
#' @param aspRatio preferred aspect ratio of the main rectangle, defined by width/height. When set to \code{NA}, the available window size is used.
#' @param vp \code{\link[grid:viewport]{viewport}} to draw in. By default it is not specified, which means that a new plot is created. Useful when drawing small multiples, or when placing a treemap in a custom grid based plot.
#' @param na.rm logical that determines whether missing values are omitted during aggregation
#' @return A list is silently returned:
#'	\item{tm}{a \code{data.frame} containing information about the rectangles}
#'  \item{vSize}{argument vSize}
#'  \item{vColor}{argument vColor}
#'	This list can be used to locate a mouse click (see \code{\link{tmLocate}}).
#' @references
#' Bederson, B., Shneiderman, B., Wattenberg, M. (2002) Ordered and Quantum Treemaps: Making Effective Use of 2D Space to Display Hierarchies. ACM Transactions on Graphics, 21(4): 833-854.
#'
#' Bruls, D.M., C. Huizing, J.J. van Wijk. Squarified Treemaps. In: W. de Leeuw, R. van Liere (eds.), Data Visualization 2000, Proceedings of the joint Eurographics and IEEE TCVG Symposium on Visualization, 2000, Springer, Vienna, p. 33-42.
#' @example ../examples/tmPlot.R
#' @import data.table
#' @import RColorBrewer
#' @import grid
#' @import colorspace
#' @export
tmPlot <-
function(dtf, 
	index, 
	vSize, 
	vColor=NULL, 
	type="value",
	title=NA,
	subtitle=NA,
	algorithm="pivotSize",
	sortID="-size",
	palette=NA,
	range=NA,
    vColorRange=NULL,
	fontsize.title=14, 
	fontsize.labels=11, 
	fontsize.legend=12,
	lowerbound.cex.labels=0.4,
	inflate.labels=FALSE,
	bg.labels= ifelse(type %in% c("value", "linked", "categorical"), "#CCCCCCAA", NA),
	force.print.labels=FALSE,
	position.legend=ifelse(type %in% c("categorical", "index", "depth"), "right", ifelse(type=="linked", "none", "bottom")),
	aspRatio=NA,
    vp=NULL,
	na.rm = FALSE) {

    require(data.table)
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

	# subtitle	
	if (!is.na(subtitle[1]) && length(subtitle) != 1) {
		warning("Length of subtitle should be 1")
		subtitle <- NA}

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
	
	if (is.na(subtitle[1])) {	
		options(warn=-1) 
		if (!is.null(vColor)) {
			if (type=="dens") 
				subtitle <- formatColorTitle(var=vColor, var2=vSize, 
                                                var2X=vColorX, div=vColorDiv)
			else
			    subtitle <- formatColorTitle(var=vColor, varX=vColorX, 
                                             div=vColorDiv)
		} else subtitle <- ""
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
			palette <- brewer.pal(11,"RdBu")
		} else if (type == "dens") {
			palette <- brewer.pal(9,"OrRd")
		} else if (type == "depth") {
			palette <- brewer.pal(8,"Set2")
		} else if (type == "index") {
		    palette <- brewer.pal(8,"Set2")
		} else if (type == "value") {
			palette <- brewer.pal(11,"RdBu")
		} else if (type == "categorical") {
			palette <- brewer.pal(12,"Set3")
		}
	} else {
		reverse <- (substr(palette[1], 1, 1)=="-")
		if (reverse) palette[1] <- substr(palette[1], 2, nchar(palette[1]))
		if ((length(palette)==1) && (palette[1] %in%row.names(brewer.pal.info))) {
			# brewer palettes
			palette <- brewer.pal(brewer.pal.info[palette, "maxcolors"], palette)
			if (reverse) palette <- rev(palette)
		} else {
			if (class(try(col2rgb(palette), silent=TRUE))=="try-error") 
				stop("color palette is not correct")
		}
	}
	
	# vColorRange
    if (!missing(vColorRange)) {
        warning("vColorRange is deprecated: use range instead.")
        range <- vColorRange
    }
    
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
	if (!is.na(bg.labels)) {
		if (class(try(col2rgb(bg.labels), silent=TRUE))=="try-error") stop("Invalid bg.labels")
	} 
		
	
	
	# force.print.labels
	if (length(force.print.labels)!=1 ||
		class(force.print.labels) !="logical")
		stop("Invalid force.print.labels")

	# position.legend
	if (!position.legend %in% c("right", "bottom", "none")) 
		stop("Invalid position.legend")	
	
	# aspRatio
	if (length(aspRatio)!=1 || (!is.na(aspRatio[1]) && !is.numeric(aspRatio)))
		stop("Invalid aspRatio")
	
	# na.rm
	if (length(na.rm)!=1 ||
		class(na.rm) !="logical")
		stop("Invalid na.rm")

	
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
    
    if (vColorX!=1) dtfDT[, c:=c*vColorX]
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
    datlist <- tmAggregate(dtfDT, indexList, type, ascending, na.rm)
    
    catLabels <- switch(type, categorical=levels(datlist$c), index=index, depth=index, NA)
    vps <- tmGetViewports(vp, fontsize.title, fontsize.labels, fontsize.legend,
                           position.legend, type, aspRatio, subtitle, catLabels)
    tmPrintTitles(vps, title, subtitle, position.legend)
    if (type == "color") {
        datlist$color <- as.character(datlist$c)
    } else {
        datlist <- tmColorsLegend(datlist, vps, position.legend, type, palette, range, indexNames=index)
    }
    datlist <- tmGenerateRect(datlist, vps, indexList, algorithm)

    tmDrawRect(datlist, vps, indexList, lowerbound.cex.labels, inflate.labels, bg.labels, force.print.labels, cex_indices)
    
    upViewport(0 + !is.null(vp))
    
	# save treemaps (indices, subindices, and coordinates), and number of rows and number of columns)
# 	tmSave <- list(tm = tm,
#                    type = type,
# 				   vSize = vSize,
# 				   vColor = vColor)
# 	invisible(tmSave)
    invisible()
}


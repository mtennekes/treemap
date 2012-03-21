#' Create treemap
#'
#' Create treemap
#'
#' @param dtf a data.frame. Required.
#' @param index	vector containing the column names in \code{dtf} that contain the aggregation indices. Required.
#' @param vSize name of the variable that determines the sizes of the rectangles. For small multiples, a vector of variable names (one for each treemap) should be given.  Required.
#' @param vColor name of the variable that, in combination with \code{type}, determines the colors of the rectangles. See \code{type} for more information. For small multiples, a vector of variable names (one for each treemap) should be given.
#' @param type the type of the treemap:
#' \describe{
#'		\item{\code{comp}:}{colors indicate change of the \code{vSize}-variable with respect to the \code{vColor}-variable (in percentages)}
#'		\item{\code{dens}:}{colors indicate density (E.g. a population density map: \code{vSize} is area size, \code{vColor} is population, and the colors are computed as densities (population per squared km's)). \code{vColor} can specified as "colName*scale factor" or "colName/scale factor" (see example below).}
#'		\item{\code{perc}:}{the \code{vColor} variable should consist of percentages between 0 and 100.}
#'		\item{\code{linked}:}{objects are linked by color over different treemaps}
#'		\item{\code{index}:}{each aggregation index has distinct color}
#'		\item{\code{value}:}{the \code{vColor}-variable is directly mapped to a color palette (by default Brewer's diverging color palette "RdYlGn").}}
#' @param title Title of the treemap. For small multiples, a vector of titles should be given. Titles are used to describe the sizes of the rectangles.
#' @param subtitle Subtitle of the treemap. For small multiples, a vector of subtitles should be given. Subtitles are used to describe the colors of the rectangles.
#' @param algorithm name of the used algorithm: "squarified" or "pivotSize". The squarified treemap algorithm (Bruls et al., 2000) produces good aspect ratios, but ignores the sorting order of the rectangles (\code{sortID}). The ordered treemap algorithm, pivot-by-size (Bederson et al., 2002) takes the sroting order (\code{sortID}) into account while aspect ratios are acceptable.
#' @param sortID name of the variable that determines the sorting order of the rectangles (from top left to bottom right). Also the values "size" and "color" can be used. To inverse the sorting order, use "-" in the prefix. By default, large rectangles are placed top left. For small multiples, a vector of variable names (one for each treemap) should be given. Only applicable when \code{algortihm=="pivotSize"}.
#' @param palette Either a color palette or a name of a Brewer palette (see \code{display.brewer.all()}). A Brewer palette can be reversed by prefixing its name with a "-".
#' @param vColorRange Range of the color variable values that is mapped to \code{palette}. Only applicable for \code{type=="value"}.
#' @param fontsize.title (maximum) font size of the title
#' @param fontsize.labels font size of the data labeling
#' @param fontsize.legend (maximum) font size of the legend
#' @param lowerbound.cex.labels number between 0 and 1 that indicates the minimum fontsize of the data labels: 0 means draw all data labels, and 1 means only draw data labels if they fit at font size \code{fontsize.data}
#' @param inflate.labels logical that determines whether data labels are inflated inside the rectangles
#' @param force.print.labels logical that determines whether data labels are being forced to be printed (also when they don't fit)
#' @param na.rm logical that determines whether missing values are omitted during aggregation
#' @return A list is silently returned:
#'	\item{tm}{List with for each treemap a \code{data.frame} containing information about the rectangles}
#'	\item{nRow}{Number of rows in the treemap grid}
#'	\item{nCol}{Number of rows in the treemap grid}
#'	This list can be used to locate a mouse click (see \code{\link{tmLocate}}).
#' @references
#' Bederson, B., Shneiderman, B., Wattenberg, M. (2002) Ordered and Quantum Treemaps: Making Effective Use of 2D Space to Display Hierarchies. ACM Transactions on Graphics, 21(4): 833-854.
#' Bruls, D.M., C. Huizing, J.J. van Wijk. Squarified Treemaps. In: W. de Leeuw, R. van Liere (eds.), Data Visualization 2000, Proceedings of the joint Eurographics and IEEE TCVG Symposium on Visualization, 2000, Springer, Vienna, p. 33-42.
#' @example ../examples/tmPlot.R
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
	vColorRange=NA,
	fontsize.title=14, 
	fontsize.labels=11, 
	fontsize.legend=12,
	lowerbound.cex.labels=0.4,
	inflate.labels=FALSE,
	force.print.labels=FALSE,
	na.rm = FALSE) {
	#############
	## Process variable names and titles
	#############
	## First checks
	if (!exists("dtf")) stop("Dataframe <dtf> not defined")
	if (!exists("index")) stop("Attribute <index> not defined")
	if (!exists("vSize")) stop("Attribute <vSize> not defined")
	if (!inherits(dtf, "data.frame")) stop("Object <dtf> is not a data.frame")
	if (any(!index %in% names(dtf))) stop("<index> contains invalid column names")


	#############
	## Internal functions
	#############

	
	formatColorTitle <- function(var, varX=NA, var2=NA, var2X=NA) {
		if (!is.na(varX)) {
			if (varX!=1) {
				var <-paste(varX,var,sep=" * ")
			}
		}
		
		if (!is.na(var2)) {
			if (var2X<1) {
				var2 <-paste(1/var2X,var2,sep=" * ")
			} else if (var2X>1) {
				var <-paste(var2X,var,sep=" * ")
			}
			var<-paste(var,"per",var2,sep=" ")
		}
		var
	}
	
	
	## Get size variable(s)
	n <- length(vSize)
	
	## Checks if all vSizes are valid
	if (!all(vSize %in% names(dtf))) stop("vSize contains invalid column names")
	classes <- sapply(dtf[, vSize, drop=FALSE],
					  FUN=function(x)class(x)[1])
	if (!all(classes %in% c("numeric", "integer")))
		stop(paste("Column(s) in ", vSize, " not numeric or integer",sep=""))
	
	
	## Checks if titles and subtitles have length n
	if (!is.na(title[1]) && length(title) != n) {
		warning(paste("Number of titles should be ", n, 
					  ". Titles will be ignored.", sep=""))
		title <- NA}
	if (!is.na(subtitle[1]) && length(subtitle) != n) {
		warning(paste("Number of subtitles should be ", n, 
					  ". Subtitles will be ignored.", sep=""))
		title <- NA}
		
	## Determine titles
	if (is.na(title[1])) {	
		options(warn=-1) 
		vSizeNames <- vSize
		options(warn=0) 
	} else {
		vSizeNames <- as.character(title)
	}
	
	vColorMplySplit <- function(vColor) {
		vColorMply <- unlist(strsplit(vColor, split="*", fixed=TRUE))
		if (length(vColorMply)==1) {
			vColorMply <- unlist(strsplit(vColor, split="/", fixed=TRUE))
			if (length(vColorMply)==1) {
				vColorMply <- c(vColorMply, 1)
			} else {
				vColorMply[2] <- (1/as.numeric(vColorMply[2]))
			}
		}
		return (vColorMply)
	}
	
	## Process formula for color variables
	if (!is.null(vColor)) {
		vColor2 <- lapply(vColor, FUN="vColorMplySplit")
		vColorX <- as.numeric(sapply(vColor2, function(x)x[2]))
		if (any(is.na(vColorX))) stop("Invalid vColor.")
		vColor <- sapply(vColor2, function(x)x[1])
	
		## Check is column names in vColor are valid
		if (!all(vColor %in% names(dtf)))
					stop("Invalid column name(s) found in vColor.")
	} else {
		vColor <- vSize #for convenience
	}
	
	##########
	## Determine grid
	##########
	
	width <- par("din")[1]
	height <- par("din")[2]
	
	mx <- n
	numbers <- matrix(rep(1:mx, mx) * rep(1:mx, each=mx), nrow=mx,ncol=mx) 
	optnum <- rep(0,mx)
	
	for (i in 1:mx) {
		optnum[i] <- min(100,which(numbers[i,]>=n))
	}
	optnum <- unique(optnum)
	optnum <- optnum[optnum!=100]
	optn <- length(optnum)
	minAsp <- 0
	for (i in 1:optn){
		rW <- optnum[i]/width
		cH <- optnum[optn+1-i]/height
		aspR <- min(rW/cH, cH/rW)
		if (aspR > minAsp) {
			minAsp <- aspR
			minAspI <- i
		}
	}

	nCol <- optnum[minAspI]
	nRow <- optnum[optn+1-minAspI]
	

	############
	## Determine sorting order
	############
	ascending <- rep(TRUE, n)
	sortID <- rep(sortID, length.out=n)

	negSort <- substr(sortID,1,1)=="-"
	ascending[negSort] <- FALSE
	sortID[negSort] <- substr(sortID[negSort],2,nchar(sortID[negSort]))
	sortID[sortID=="size"] <- vSize[sortID=="size"]
	sortID[sortID=="color"] <- vSize[sortID=="color"]
	
	
	############
	## Determine color palette
	############
	
	
	if (is.na(palette[1])) {
		if (type == "comp") {
			palette <- brewer.pal(11,"RdBu")
		} else if (type == "perc") {
			palette <- brewer.pal(9,"Blues")
		} else if (type == "dens") {
			palette <- brewer.pal(9,"OrRd")
		} else if (type == "linked") {
			palette <- c(brewer.pal(12,"Set3"),
						 brewer.pal(8,"Set2")[c(1:4,7,8)],
						 brewer.pal(9,"Pastel1")[c(1,2,4,5)])
		} else if (type == "index") {
			palette <- brewer.pal(8,"Set2")
		} else if (type == "value") {
			palette <- brewer.pal(11,"RdYlGn")
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
	
	
	
	############
	## Determine legend
	############
	legenda <- (type!="linked" && type!="index")
	
	## Determine subtitles
	if (is.na(subtitle)) {	
		options(warn=-1) 
		if (!is.null(vColor)) {
			if (type=="dens") 
				vColorNames <- mapply(FUN="formatColorTitle", var=vColor, var2=vSize, var2X=vColorX)
			else
				vColorNames <- mapply(FUN="formatColorTitle", var=vColor, varX=vColorX)
		} else vColorNames <- rep("",n)
		options(warn=0) 
	} else {
		vColorNames <- as.character(subtitle)
	}
	
	
	
	###########
	## Aggregate
	###########
	vars <- unique(c(vSize, vColor))
	
	dtfDT <- as.data.table(dtf)
	setkeyv(dtfDT, index)
	
	.SD <- NULL; rm(.SD); #trick R CMD check
	dat <- dtfDT[ , lapply(.SD[, vars, with=FALSE], sum, na.rm=na.rm), by=index]
	
	#dat <- ddply(dtf, index, colwise(sum, vars), na.rm=na.rm)
	
	depth <- length(index)
	indexList <- paste("index", 1:depth, sep="")
	
	setnames(dat, 1:depth, indexList)

	minima <- sapply(dat[, -(1:depth), with=FALSE], min)
	if (any(is.na(minima)))
		stop(paste("Column(s) ",
				   paste(names(minima)[is.na(minima)],
collapse=", "), " contain missing values.", sep=""))
	
	if (min(minima[vSize]) < 0)
		stop(paste("Column(s) ",
				   paste(names(minima[vSize])[minima[vSize]<0], collapse=", "),
				   " contain negative values.", 
				   sep=""))

	scaledInd <- which(vColorX!=1)
	for (i in scaledInd) {
		colName <- paste(vColor[i], vColorX[i], sep="__")
		dat[[colName]] <- dat[[vColor[i]]] / vColorX[i]
		vColor[i] <- colName
	}
	
	############
	## Plot treemap(s)
	############
	grid.newpage()
	
	pushViewport(viewport(name="grid",layout=grid.layout(nRow, nCol)))

	iCol<-1
	iRow<-1
	tm<-list()
	for (i in 1:n) {
		#browser()
		dat_i <- data.table(value=dat[[vSize[i]]],
							value2=dat[[vColor[i]]],
							sortInd=dat[[sortID[i]]])
		dat_i <- cbind(dat_i, dat[, indexList, with=FALSE])

		if (!ascending[i]) {
			dat_i[["sortInd"]] <- -dat_i[["sortInd"]]
		}

		pushViewport(viewport(name=paste("tm",i,sep=""),layout.pos.col=iCol, layout.pos.row=iRow))
		tm[[i]] <- baseTreemap(
			dat=dat_i,
			type=type,
			algorithm=algorithm,
			legenda=legenda,
			sizeTitle=vSizeNames[i],
			colorTitle=vColorNames[i],
			palette=palette,
			vColorRange=vColorRange,
			fontsize.title=fontsize.title, 
			fontsize.labels=fontsize.labels, 
			fontsize.legend=fontsize.legend,
			lowerbound.cex.labels=lowerbound.cex.labels,
			inflate.labels=inflate.labels,
			force.print.labels=force.print.labels)
			
		upViewport()
		iRow<-iRow+1
		if (iRow>nRow) {
			iRow<-1
			iCol<-iCol+1
		}	
	}
	
	# go to root viewport (from grid layout)
	upViewport()

	# save treemaps (indices, subindices, and coordinates), and number of rows and number of columns)
	tmSave <- list()
	tmSave$tm <- tm
	tmSave$nRow <- nRow
	tmSave$nCol <- nCol
	invisible(tmSave)
}


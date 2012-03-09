#' User-friendly treemap function
#'
#' User-friendly treemap function
#'
#' For the arguments \code{vSize} and \code{vColor}, use the following formula syntax:
#'		\itemize{
#'		\item one treemap
#'			\itemize{
#'			\item \code{vSize = <variable name>}
#'			\item \code{vColor = <variable name>/<scale>*<variable name>} second part (after /), useful for density treemaps, is optional when treemap type is "linked", this formula has no use}
#'		\item multiple treemaps	formulas for each treemap are seperated with +}
#' 
#' @param dtf a data.frame (required).
#' @param index	a character vector containing the column names in \code{dtf} that contain the indices (required).
#' @param vSize character containing the formula of the variables that determine the sizes (required). For details about the syntax see below.
#' @param vColor a character containing the formula of the variables that determine the colors. For details about the syntax see below.
#' @param sortID the name of the column in \code{dtf} on which the rectangles should be sorted (from top left to bottom right). To inverse the sorting order, use "-" in the prefix.
#' @param type the type of the treemap:
#' \itemize{
#'		\item \code{auto}	automatic determination of type (default setting)
#'		\item \code{dens}	density treemap (dense areas get darker colors)
#'		\item \code{comp} comparison treemap (colors are used to compare variables)
#'		\item \code{perc}	treemap (color variable is in percentages)
#'		\item \code{linked} each index has an own, distinctive, color (useful to compare small multiples)
#'		\item \code{value}	treemap where values of the color variable are directly mapped to a color palette. By default a diverging color scale (Brewer's "RdYlGn") is used where negative values are red and positive green. By setting \code{palette} (in combination with \code{vColorRange}), any color palette can be used.}
#' @param titles A character vector containing the title(s) of the treemap(s) (optional). Use this for describing the sizes of the rectangles.
#' @param subtitles A character vector containing the subtitle(s) of the treemap(s) (optional). Use this for describing the colors of the rectangles.
#' @param palette Either a color palette or a name of a Brewer palette (see \code{display.brewer.all()})
#' @param vColorRange Range of the color variable values that is mapped to \code{palette}. Only applicable for \code{type="value"}.
#' @param fontsize.title (maximum) font size of the title
#' @param fontsize.labels font size of the data labeling
#' @param fontsize.legend (maximum) font size of the legend
#' @param lowerbound.cex.labels number between 0 and 1 that indicates the minimum fontsize of the data labels: 0 means draw all data labels, and 1 means only draw data labels if they fit at font size \code{fontsize.data}
#' @param inflate.labels boolean that determines whether data labels are inflated inside the rectangles
#' @param force.print.labels boolean that determines whether data labels are being forced to be printed (also when they don't fit)
#' @return A list is silently returned:
#'	\item{tm}{List with for each treemap a \code{data.frame} containing information about the rectangles}
#'	\item{nRow}{Number of rows in the treemap grid}
#'	\item{nCol}{Number of rows in the treemap grid}
#'	This list can be used to locate a mouse click (see \code{\link{tmLocate}}).
#' @example ../examples/tmPlot.R
#' @export
tmPlot <-
function(dtf, 
	index, 
	vSize, 
	vColor="", 
	sortID="",
	type="auto",
	titles=NA,
	subtitles=NA,
	palette=NA,
	vColorRange=NA,
	fontsize.title=14, 
	fontsize.labels=11, 
	fontsize.legend=12,
	lowerbound.cex.labels=0.4,
	inflate.labels=FALSE,
	force.print.labels=FALSE) {
	#############
	## Process variable names and titles
	#############
	## First checks
	if (!exists("dtf")) stop("Dataframe <dtf> not defined")
	if (!exists("index")) stop("Attribute <index> not defined")
	if (!exists("vSize")) stop("Attribute <vSize> not defined")
	if (class(dtf)!="data.frame") stop("Object <dtf> is not a data.frame")
	if (any(!index %in% names(dtf))) stop("<index> is not a column name of <dtf>")


	#############
	## Internal functions
	#############
	formatTitle <- function(x) {

			isnumeric <- function(s) !is.na(as.numeric(s))
			
			s <- strsplit(x, " ")[[1]]
			string <- paste(toupper(substring(s, 1,1)), substring(s, 2),
			  sep="", collapse=" ")

			if (isnumeric(substring(string,nchar(string)-1,nchar(string)))&&
			!isnumeric(substring(string,nchar(string)-2,nchar(string)))) {
			string <- paste(substring(string,1,nchar(string)-2)," '",substring(string,nchar(string)-1,nchar(string)),sep="")
		} else if (isnumeric(substring(string,nchar(string)-3,nchar(string)))&&
			!isnumeric(substring(string,nchar(string)-4,nchar(string)))) {
			string <- paste(substring(string,1,nchar(string)-4),substring(string,nchar(string)-3,nchar(string)),sep=" ")
		}
		string	
	}
	
	formatColorTitle <- function(sx,x,sdiv,div) {
		string <- formatTitle(x)
		if (sx!=1) {	
			string<-paste(sx,string,sep=" ")
		}
		if (!is.na(div)) {
			stringDiv <- formatTitle(div)
			if (sdiv!=1) {
				stringDiv<-paste(sdiv,stringDiv,sep=" ")
			}
			string<-paste(string,"per",stringDiv,sep=" ")
		}
		string
	}
	
	vColorDivSplit <- function(vColor) {
		vColorDiv <- unlist(strsplit(vColor, split="/", fixed=TRUE))
		return (vColorDiv)
	}
	
	vColorMplySplit <- function(vColor) {
		vColorMply <- unlist(strsplit(vColor, split="*", fixed=TRUE))
		if (length(vColorMply)==1) {
			vColorMply <- c(1,vColorMply)
		}
		return (vColorMply)
	}


	
	## Get size variable(s)
	vSizeVector <- unlist(strsplit(vSize, split="+", fixed=TRUE))
	n <- length(vSizeVector)

	## Checks if all vSizes are valid
	for (i in 1:n) {
		if (!vSizeVector[i] %in% names(dtf)) stop(paste(vSizeVector[i]," is not a column in <dtf>", sep=""))
		if (class(dtf[,vSizeVector[i]])!="numeric" && class(dtf[,vSizeVector[i]])!="integer") stop(paste("Column ", vSizeVector[i], " is not numeric or integer",sep=""))
		if (any(is.na(dtf[,vSizeVector[i]]))) stop(paste("Column ", vSizeVector[i], " contains missing values.",sep=""))
		if (min(dtf[,vSizeVector[i]])<0) stop(paste("Column ", vSizeVector[i], " contains negative values.",sep=""))
	}

	## Checks if titles and subtitles have length n
	if (!is.na(titles[1]) && length(titles) != n) {warning(paste("Number of titles should be ", n, ". Titles will be ignored.", sep="")); titles <- NA}
	if (!is.na(subtitles[1]) && length(subtitles) != n) {warning(paste("Number of subtitles should be ", n, ". Subtitles will be ignored.", sep="")); titles <- NA}
		
	## Determine titles
	if (is.na(titles[1])) {	
		options(warn=-1) 
		vSizeNames <- mapply(FUN="formatTitle", vSizeVector)
		options(warn=0) 
	} else {
		vSizeNames <- as.character(titles)
	}

	## Process formula for color variables
	vColorAdd <- unlist(strsplit(vColor, split="+", fixed=TRUE))
	vColorDiv <- unlist(mapply(FUN="vColorDivSplit", vColorAdd))
			
	if (vColor=="") {
		vColorVector <- matrix(data=NA,nrow=2,ncol=n)
		vColorVectorBy <- matrix(data=NA,nrow=2,ncol=n)
	} else if (is.vector(vColorDiv)) {
		vColorVector <- unlist(mapply(FUN="vColorMplySplit", vColorDiv))
		vColorVectorBy <- matrix(data=NA,nrow=2,ncol=n)
	} else {
		vColorVector <- unlist(mapply(FUN="vColorMplySplit", vColorDiv[1,]))
		if (is.matrix(vColorVector)) {
			vColorVectorBy <- unlist(mapply(FUN="vColorMplySplit", vColorDiv[2,]))
		}
	}

	## Determine subtitles
	if (is.na(subtitles)) {	
		options(warn=-1) 
		if (!all(is.na(vColorVector))) {
			vColorNames <- mapply(FUN="formatColorTitle", vColorVector[1,],vColorVector[2,],vColorVectorBy[1,],vColorVectorBy[2,])
		} else vColorNames <- rep("",n)
		options(warn=0) 
	} else {
		vColorNames <- as.character(subtitles)
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
	ascending <- TRUE
	if (substr(sortID,1,1)=="-") {
		ascending <- FALSE
		sortID <- substr(sortID,2,nchar(sortID))
	}

	############
	## Determine treemap type
	############
	
	legenda <- TRUE
	if (type=="auto") {
		if (all(is.na(vColorVector))) {
			type <- "linked"
			legenda <- FALSE
		} else if (!all(is.na(vColorVectorBy))) {
			type <- "dens"
		} else {
			perc <- ((dtf[vSizeVector] - dtf[vColorVector[2,]] )/dtf[vColorVector[2,]])*100
			isNaN <- apply(perc, 2, FUN=is.nan)
			perc[isNaN] <- 0
			
			if (min(perc)<=-60|| max(perc)>=150) {
				if (min(dtf[vSizeVector])>=0 && max(dtf[vSizeVector])<=100) {
					type <- "perc"	
				} else type <- "value"
			} else type <- "comp"
		}
	} else if (type=="linked") {
		legenda <- FALSE
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
		datSize<-as.numeric(dtf[[vSizeVector[i]]])
		if (all(is.na(vColorVector))) { 
			datColor<-dtf[[vSizeVector[1]]]
		} else {
			datColor<-dtf[[vColorVector[2,i]]]/as.numeric(vColorVector[1,i])
			if (!is.na(vColorVectorBy[i])) {
				datColor<-datColor/(dtf[[vColorVectorBy[2,i]]]/as.numeric(vColorVectorBy[1,i]))
			}
		}
		pushViewport(viewport(name=paste("tm",i,sep=""),layout.pos.col=iCol, layout.pos.row=iRow))
#		grid.rect(gp=gpar(col="red"))
		if (sortID=="size") {
			sortDat <- datSize
		} else if (sortID=="color") {
			sortDat <- datColor
		} else if (sortID=="") {
			sortDat <- datSize #NA
		} else {
			sortDat <- dtf[sortID]
		}
		if (!ascending) {
			sortDat <- -sortDat
		}
		dat<-data.frame(value=datSize, value2=datColor, sortInd=sortDat)
		names(dat) <- c("value", "value2", "sortInd")
		for (j in 1:length(index)) {
			indName <- paste("index", j, sep="")
			dat[[indName]] <- dtf[[index[j]]]
		}		

		tm[[i]] <- baseTreemap(
			dat=dat,
			type=type,
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


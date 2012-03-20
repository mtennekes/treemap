#' User-friendly treemap function
#'
#' User-friendly treemap function
#'
#' For the argument \code{vColor}, use the following formula syntax:
#'		\itemize{
#'		\item one treemap
#'			\itemize{
#'			\item \code{vSize = <variable name>}
#'			\item \code{vColor = <scale>*<variable name>} 
#'          The second part (starting with /) is optional. This part is useful for density treemaps.}
#'		\item multiple treemaps: formulas are seperated with +}
#' 
#' @param dtf a data.frame (required).
#' @param index	character vector containing the column names in \code{dtf} that contain the aggregation indices (required). 
#' @param vSize character vector of variable names (one for each treemap) that determine the sizes (required).
#' @param vColor character vector of variable names (one for each treemap) that determine the colors. For details about the syntax see below.
#' @param sortID character vector of variable names (one for each treemap) that determine the sorting order of the rectangles (from top left to bottom right). Also the values "size" and "color" can be used. To inverse the sorting order, use "-" in the prefix. By default, large rectangles are placed top left.
#' @param type the type of the treemap:
#' \describe{
#'		\item{\code{comp}:}{colors indicate change of the \code{vSize}-variable with respect to the \code{vColor}-variable (in percentages)}
#'		\item{\code{dens}:}{colors indicate density (like a population density map): \code{vColor} should be defined as something (e.g.\ population) per unit of \code{vSize} (e.g.\ area size)}
#'		\item{\code{perc}:}{the \code{vColor} variable should consist of percentages between 0 and 100.}
#'		\item{\code{linked}:}{objects are linked by color over different treemaps}
#'		\item{\code{value}:}{the \code{vColor}-variable is directly mapped to a color palette (by default Brewer's diverging color palette "RdYlGn").}}
#' @param titles A character vector containing the title(s) of the treemap(s). Use this for describing the sizes of the rectangles.
#' @param subtitles A character vector containing the subtitle(s) of the treemap(s). Use this for describing the colors of the rectangles.
#' @param palette Either a color palette or a name of a Brewer palette (see \code{display.brewer.all()}).
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
#' @example ../examples/tmPlot.R
#' @export
tmPlot <-
function(dtf, 
	index, 
	vSize, 
	vColor=NULL, 
	sortID="-size",
	type="value",
	titles=NA,
	subtitles=NA,
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
	formatTitle <- function(x) {

			isnumeric <- function(s) !is.na(as.numeric(s))
			
			s <- strsplit(x, " ")[[1]]
			string <- paste(toupper(substring(s, 1,1)), substring(s, 2),
			  sep="", collapse=" ")

			if (isnumeric(substring(string,nchar(string)-1,nchar(string)))
				&&	!isnumeric(
					substring(string,nchar(string)-2,nchar(string)))) {
				string <- paste(substring(string,1,nchar(string)-2),
								" '",substring(string,nchar(string)-1,
											   nchar(string)),sep="")
		} else if (isnumeric(substring(string,nchar(string)-3,
									   nchar(string)))
				   && !isnumeric(substring(string, nchar(string)-4,
								 nchar(string)))) {
			string <- paste(substring(string,1,nchar(string)-4),
							substring(string,nchar(string)-3,
									  nchar(string)),sep=" ")
		}
		string	
	}
	
	formatColorTitle <- function(x, div=NA, sdiv=NA) {
		string <- formatTitle(x)
		if (!is.na(div)) {
			stringDiv <- formatTitle(div)
			if (sdiv!=1) {
				stringDiv<-paste(1/sdiv,stringDiv,sep=" ")
			}
			string<-paste(string,"per",stringDiv,sep=" ")
		}
		string
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
	if (!is.na(titles[1]) && length(titles) != n) {
		warning(paste("Number of titles should be ", n, 
					  ". Titles will be ignored.", sep=""))
		titles <- NA}
	if (!is.na(subtitles[1]) && length(subtitles) != n) {
		warning(paste("Number of subtitles should be ", n, 
					  ". Subtitles will be ignored.", sep=""))
		titles <- NA}
		
	## Determine titles
	if (is.na(titles[1])) {	
		options(warn=-1) 
		vSizeNames <- mapply(FUN="formatTitle", vSize)
		options(warn=0) 
	} else {
		vSizeNames <- as.character(titles)
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
		vColor2 <- lapply(FUN="vColorMplySplit", vColor)
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
	## Determine legend
	############
	legenda <- (type!="linked" && type!="index")
	
	## Determine subtitles
	if (is.na(subtitles)) {	
		options(warn=-1) 
		if (!is.null(vColor)) {
			if (type=="dens") 
				vColorNames <- mapply(FUN="formatColorTitle", vColor, vSize, vColorX)
			else
				vColorNames <- mapply(FUN="formatColorTitle", vColor)
		} else vColorNames <- rep("",n)
		options(warn=0) 
	} else {
		vColorNames <- as.character(subtitles)
	}
	
	
	###########
	## Aggregate
	###########
	vars <- unique(c(vSize, vColor))
	
	dtfDT <- as.data.table(dtf)
	setkeyv(dtfDT, index)
	
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
	if (min(minima) < 0)
		stop(paste("Column(s) ",
				   paste(names(minima)[minima<0], collapse=", "),
				   " contain negative values.", 
				   sep=""))

	
	
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


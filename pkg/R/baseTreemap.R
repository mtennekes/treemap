#' Plots a single treemap
#'
#' This function that plots a single treemap. It is used by the more user-friedly function \code{\link{tmPlot}}.
#'
#' @param dat data frame containing the columns:
#'	\itemize{
#'	\item \code{values} numerical vector defining rectangle sizes
#'	\item \code{values2} numerical vector defining rectangle colors
#'	\item \code{sortInd} vector defining the order of the rectangles (from top left to bottom right)
#'	\item \code{index1}...\code{index<k>} one or more vectors defining the aggregation indices}
#' @param type the type of the treemap (optional):
#'		\itemize{
#'		\item \code{auto} automatic determination of type (default setting)
#'		\item \code{dens} density treemap (dense areas get darker colors)
#'		\item \code{comp} comparison treemap (colors are used to compare variables)
#'		\item \code{perc} treemap (color variable is in percentages)
#'		\item \code{linked} each index has an own, distinctive, color (useful for small multiples)}
#' @param width width in inches used to determine text sizes (optional)
#' @param height height in inches used to determine text sizes (optional)
#' @param neg boolean to determine whether color values are interpreted negatively (optional)
#' @param legenda boolean, determines whether legenda should be plot (optional)
#' @param upperboundText upperbound of the text sizes (optional)
#' @param lowerboundText lowerbound of the text sizes (optional)
#' @param forcePrint boolean, determines whether text label should be print, even if they don't fit (optional)
#' @param sizeTitle main title (optional)
#' @param colorTitle subtitle (optional)
#' @export
baseTreemap <-
function(dat,
	type="fixed",
	width=convertWidth(unit(1,"npc"),"inches",valueOnly = TRUE),
	height=convertHeight(unit(1,"npc"),"inches",valueOnly = TRUE),
	neg=FALSE,
	legenda=TRUE,
	upperboundText=0.8, 
	lowerboundText=0.4, 
	forcePrint=FALSE,
	sizeTitle="", 
	colorTitle="") {

	plotMargin <- unit(0.5,"cm")
	
	cexLarge <- min(14,(height*3.6), (width*3.6))
	cexSmall <- cexLarge * 0.8

	# Determine legenda viewports
	if (legenda) {
		legWidth <- min(unit(5, "inches"), convertWidth(unit(0.9, "npc")-2*plotMargin,"inches"))
		legHeight <- unit(cexSmall * 0.06, "inches")
		
		vpLeg <- viewport(name = "legenda",
		  x = plotMargin,
		  y = 0.5*plotMargin,
		  width = unit(1, "npc") - 2 * plotMargin,
		  height = legHeight,
		  gp=gpar(fontsize=cexSmall),
		  just = c("left", "bottom"))
	
		vpLeg2 <- viewport(name = "legenda2",
		  x = (unit(1, "npc") - legWidth)*0.5,
		  y = legHeight*0.3,
		  width = legWidth,
		  height = legHeight*0.7,
		  gp=gpar(fontsize=cexSmall),
		  just = c("left", "bottom"))
	} else legHeight <- unit(0,"inches")

	# Determine treemap viewports
	vpDat <- viewport(name = "dataregion", 
	  x = plotMargin,
	  y = legHeight + 0.5*plotMargin,
	  width = unit(1, "npc") - 2 * plotMargin,
	  height = unit(1,"npc") - legHeight - plotMargin,
	  gp=gpar(fontsize=cexLarge),
	  just = c("left", "bottom"))
	
	vpDat2 <- viewport(name = "dataregion2", 
	  x = 0,
	  y = 0,
	  width = unit(1, "npc"),
	  height = unit(1,"npc") - unit(1.5,"lines"),
	  gp=gpar(fontsize=cexSmall),
	  just = c("left", "bottom"))
	
	
	#determine depth
	dat <- dat[dat$value>0,]
	depth <- sum(substr(colnames(dat),1,5)=="index")

	dat$dlevel <- apply(dat[paste("index", 1:depth, sep="")], MARGIN=1, FUN=function(x, d){d-pmax(sum(is.na(x)), sum(x=="", na.rm=TRUE))}, depth)

	
	dats <- list()

	datV <- data.frame(value=numeric(0), value2=numeric(0))
	for (i in 1:depth) {
		indexList <- paste("index", 1:i, sep="")
		if (type=="dens") {
			dat$value2abs <- dat$value * dat$value2
			value <- NULL; rm(value)
			value2abs <- NULL; rm(value2abs)
			sortInd <- NULL; rm(sortInd)
			dats[[i]] <- ddply(dat, indexList, colwise(sum, .(value, value2abs, sortInd)))
			
			dats[[i]]$value2 <- dats[[i]]$value2abs / dats[[i]]$value
			dats[[i]]$value2abs <- NULL
			dats[[i]]$value2abs <- NULL
		} else {
			value <- NULL; rm(value)
			value2 <- NULL; rm(value2)
			sortInd <- NULL; rm(sortInd)
			dats[[i]] <- ddply(dat, indexList, colwise(sum, .(value, value2, sortInd)))
		}
		dats[[i]] <- unique(merge(dats[[i]], dat[c(indexList, "dlevel")], by=indexList))
		dats[[i]]$clevel <- i
		dats[[i]] <- dats[[i]][order(dats[[i]]$sortInd),]
		datV <- rbind(datV, dats[[i]][c("value", "value2", "index1")])
	}
	

	# Show legenda and determine colors
	if (legenda) {	
		pushViewport(vpLeg)
#		grid.rect()
		grid.text(colorTitle, y = unit(0.5, "lines"))
		pushViewport(vpLeg2)
#		grid.rect()
	}
	
	if (type == "comp") {
		datV$color <- comp2col(datV, upperboundText, legenda, neg)
	} else if (type == "perc") {
		datV$color <- fill2col(datV, upperboundText, legenda, neg)
	} else if (type == "dens") {
		datV$color <- dens2col(datV, upperboundText, legenda, neg)
	} else if (type == "linked") {
		datV$color <- fixed2col(datV)
	}
	if (legenda) {	
		upViewport()
		upViewport()
	}
	
	datL <- sapply(dats, FUN=nrow)
	datL1 <- cumsum(c(1, datL[-depth]))
	datL2 <- (datL1 + datL) - 1
	for (i in 1:depth) {
		dats[[i]]$color <- datV[datL1[i]:datL2[i],"color"]
	}
	
	
	pushViewport(vpDat)

	grid.text(sizeTitle, y = unit(1, "npc") - unit(0.5, "lines"))
	pushViewport(vpDat2)
	grid.rect(name="TMrect")
		
	# Determine window size
	dataRec <- list(X0=0,Y0=0,
		W=convertWidth(unit(1, "grobwidth", "TMrect"),"inches",valueOnly=TRUE),
		H=convertHeight(unit(1, "grobheight", "TMrect"),"inches",valueOnly=TRUE))
	
	
	findRecs <- function(dat2, level, rec, dats) {
		recDat <- pivotSize(dat2, rec)
		#recDat$level <- level
		tempDat <- dat2[c(1,(ncol(dat2)-2):ncol(dat2))]
		names(tempDat) <- c("ind" , "dlevel", "clevel", "color")
		recDat <- merge(tempDat, recDat, by="ind")
		
		recSel <- recDat[recDat$dlevel > recDat$clevel,]
	
		n <- nrow(recSel)
		if (n!=0) {for (i in 1:n) {
			smallRec <- recSel[i, c("x0", "y0", "w", "h")]
			datSel <- dats[[level+1]][dats[[level+1]][paste("index", level, sep="")]==as.character(recSel[i,"ind"]),c(paste("index", level+1, sep=""),"value", "value2", "dlevel", "clevel", "color")]
			recDat <- rbind(recDat, findRecs(datSel, level+1, smallRec, dats))
		}}
		return(recDat)
	}
	
	recList <- findRecs(dats[[1]], 1, dataRec, dats)
	
#browser()

	# convert to npc (0 to 1)
	recList$x0 <- recList$x0 / dataRec$W
	recList$y0 <- recList$y0 / dataRec$H
	recList$w <- recList$w / dataRec$W
	recList$h <- recList$h / dataRec$H
	
	drawRecs <- function(recs) {
		if (!is.na(recs$recs)[1]) grid.draw(recs$recs)
		if (!is.na(recs$txtbg)[1]) grid.draw(recs$txtbg)
		if (!is.na(recs$txt)[1]) grid.draw(recs$txt)
	}

	if (depth==1) {
		whichFill <- rep(TRUE, nrow(recList))
		recs_fill <- createRec(recList, filled=TRUE, label="normal", labellb=lowerboundText, lwd = 1)
		grid.draw(recs_fill$recs)
		grid.draw(recs_fill$txt)
	} else {
		whichBold <- recList$clevel==1
		lwds <- depth - recList$clevel + 1
		whichFill <- recList$clevel==recList$dlevel
		
		recs_fill_bold <- createRec(recList[whichFill & whichBold,], filled=TRUE, label="bold", labellb=lowerboundText, lwd = lwds[whichFill & whichBold])
		recs_fill_norm <- createRec(recList[whichFill & !whichBold,], filled=TRUE, label="normal", labellb=lowerboundText, lwd = lwds[whichFill & !whichBold])
		
		recs_trans_bold <- createRec(recList[!whichFill & whichBold,], filled=FALSE, label="bold", labellb=lowerboundText, labelbg = TRUE, lwd = lwds[!whichFill & whichBold]) 
		recs_trans_norm <- createRec(recList[!whichFill & !whichBold,], filled=FALSE, label="normal", labellb=lowerboundText, lwd = lwds[!whichFill & !whichBold]) 
		
		cover <- overlap(recs_fill_norm$txtbg, recs_trans_norm$txtbg)
		if (!is.na(cover[1])) {
			recs_fill_norm$txt$gp$col[cover] <- NA
			recs_fill_norm$bg$gp$fill[cover] <- NA
		}
		cover <- overlap(recs_fill_norm$txtbg, recs_trans_bold$txtbg)
		if (!is.na(cover[1])) {
			recs_fill_norm$txt$gp$col[cover] <- NA
			recs_fill_norm$bg$gp$fill[cover] <- NA
		}
		cover <- overlap(recs_fill_bold$txtbg, recs_trans_bold$txtbg)
		if (!is.na(cover[1])) {
			recs_fill_bold$txt$gp$col[cover] <- NA
			recs_fill_bold$bg$gp$fill[cover] <- NA
		}
	
		drawRecs(recs_fill_norm)
		drawRecs(recs_fill_bold)
		drawRecs(recs_trans_norm)
		drawRecs(recs_trans_bold)
		
	}
		
		#lowMerge <- merge(dat, recList_low, by.x="subindex", by.y="ind")
		#highMerge <- merge(dat, recList_high_fill, by.x="index", by.y="ind")
		#resultDat <- rbind(highMerge,lowMerge)[,c("index", "subindex", "x0", "y0", "w",		"h")]
		
		resultDat <- recList[whichFill, c("ind", "clevel", "x0", "y0", "w", "h")]
		

	upViewport()
	upViewport()
	return(resultDat)
}


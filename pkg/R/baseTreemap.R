baseTreemap <-
function(dat,
	type,
	algorithm,
	legenda,
	sizeTitle, 
	colorTitle,
	palette,
	vColorRange,
	fontsize.title, 
	fontsize.labels, 
	fontsize.legend, 
	lowerbound.cex.labels,
	inflate.labels,
	force.print.labels,
	cex_indices,
	indexNames,
	aspRatio) {

	# determine available plot width and height
	width <- convertWidth(unit(1,"npc"), "inches",valueOnly = TRUE)
	height <- convertHeight(unit(1,"npc"), "inches",valueOnly = TRUE)
	
	plotMargin <- unit(0.5,"cm")
	
	# determine fontsizes
	fsTitle <- min(fontsize.title, (height*3.6), (width*3.6))
	fsData <- min(fontsize.labels, (height*3.6), (width*3.6))
	fsLegend <- min(fontsize.legend, (height*3.6), (width*3.6))
	
	# Determine legenda viewports
	if (legenda) {
		legWidth <- min(unit(5, "inches"), convertWidth(unit(0.9,
															 "npc")-2*plotMargin,"inches"))
		legHeight <- unit(fsLegend * 0.06, "inches")
		
		vpLeg <- viewport(name = "legenda",
		  x = plotMargin,
		  y = 0.5*plotMargin,
		  width = unit(1, "npc") - 2 * plotMargin,
		  height = legHeight,
		  gp=gpar(fontsize=fsLegend),
		  just = c("left", "bottom"))
	
		vpLeg2 <- viewport(name = "legenda2",
		  x = (unit(1, "npc") - legWidth)*0.5,
		  y = legHeight*0.3,
		  width = legWidth,
		  height = legHeight*0.7,
		  gp=gpar(fontsize=fsLegend),
		  just = c("left", "bottom"))
	} else legHeight <- unit(0,"inches")

	# Determine treemap viewports
	vpDat <- viewport(name = "dataregion", 
	  x = plotMargin,
	  y = legHeight + 0.5*plotMargin,
	  width = unit(1, "npc") - 2 * plotMargin,
	  height = unit(1,"npc") - legHeight - plotMargin,
	  gp=gpar(fontsize=fsTitle),
	  just = c("left", "bottom"))

	vpDat2 <- viewport(name = "dataregion2", 
					   x = 0,
					   y = 0,
					   width = unit(1, "npc"),
					   height = unit(1, "npc") - unit(1.5, "lines"),
					   gp=gpar(fontsize=fsData),
					   just = c("left", "bottom"))
	
	#determine depth
	depth <- sum(substr(colnames(dat),1,5)=="index")
#browser()
	dats <- list()
	datV <- data.table(value=numeric(0), value2=numeric(0), index=character(0), level=integer(0))
	for (i in 1:depth) {
		indexList <- paste("index", 1:i, sep="")
		value <- NULL; rm(value)
		value2 <- NULL; rm(value2)
		sortInd <- NULL; rm(sortInd)
		dats_i <- dat[, lapply(.SD[, list(value, value2, sortInd)], sum), by=indexList]
		
		## remove rectangles of size 0
		dats_i <- dats_i[dats_i$value>0, ]

		if (type=="dens") {
			dats_i$value2 <- dats_i$value2 / dats_i$value
			dats_i$value2[is.nan(dats_i$value2)] <- 0
		}
		
		dats_i$clevel <- i
		dats_i$level <- ifelse(is.na(dats_i[[paste("index", i, sep="")]]),
							   dats[[i-1]][["level"]][match(
							   	dats_i[[paste("index", i-1, sep="")]],
							   	dats[[i-1]][[paste("index", i-1, sep="")]]
							   	)], i)
		dats_i <- dats_i[order(dats_i$sortInd),]
		
		
		
		datV <- rbind(datV, 
					  dats_i[, list(value, value2,
					  			  index=do.call(paste, 
					  			  			  c(dats_i[, indexList, with=FALSE], sep="__")), 
					  			  level)])
		dats[[i]] <- dats_i
	}
	
	# Show legenda and determine colors
	if (legenda) {	
		pushViewport(vpLeg)
		grid.text(colorTitle, y = unit(0.5, "lines"))
		pushViewport(vpLeg2)
	}
	
	if (type == "comp") {
		datV$color <- comp2col(datV, legenda, palette)
	} else if (type == "dens") {
		datV$color <- dens2col(datV, legenda, palette) 
	} else if (type == "linked") {
		datV$color <- fixed2col(datV, palette)
	} else if (type == "index") {
		datV$color <- index2col(datV, palette)
	} else if (type == "value") {
		datV$color <- value2col(datV, legenda, palette, vColorRange)
	}
	if (legenda) {	
		upViewport()
		upViewport()
	}
	
	datL <- sapply(dats, FUN=nrow)
	datL1 <- cumsum(c(1, datL[-depth]))
	datL2 <- (datL1 + datL) - 1
	for (i in 1:depth) {
		dats[[i]]$color <- datV[datL1[i]:datL2[i], color]
	}
	
	pushViewport(vpDat)
	grid.text(sizeTitle, y = unit(1, "npc") - unit(0.5, "lines"))
	pushViewport(vpDat2)
	
	datWidth <- convertWidth(unit(1,"npc"), 
							 "inches", valueOnly=TRUE)
	datHeight <- convertHeight(unit(1,"npc"),
							   "inches", valueOnly=TRUE)
	aspWindow <- datWidth / datHeight
	
	if (!is.na(aspRatio)) {
		if (aspRatio < aspWindow) {
			datWidth <- datHeight * aspRatio
		} else if (aspRatio > aspWindow) {
			datHeight <- datWidth / aspRatio
		}
	}
	
	vpDat3 <- viewport(name = "dataregion3", 
					   x = unit(0.5, "npc"),
					   y = unit(0.5, "npc"),
					   width = unit(datWidth, "inches"),
					   height = unit(datHeight,"inches"),
					   gp=gpar(fontsize=fsData),
					   just = c("centre", "centre"))
	
	pushViewport(vpDat3)
	grid.rect()

	dataRec <- data.table(X0=0, Y0=0, W=datWidth, H=datHeight)
	recList <- data.table(ind=factor(NULL), 
						  clevel=numeric(0),
						  color=character(0),
						  x0=numeric(0),
						  y0=numeric(0),
						  w=numeric(0),
						  h=numeric(0),
						  fullname=character(0))

	
	dats[[1]] <- cbind(dats[[1]], dataRec[rep(1,nrow(dats[[1]]))])	
	x0 <- NULL; rm(x0); #trick R CMD check
	y0 <- NULL; rm(y0); #trick R CMD check
	w <- NULL; rm(w); #trick R CMD check
	h <- NULL; rm(h); #trick R CMD check

	X0 <- NULL; rm(X0); #trick R CMD check
	Y0 <- NULL; rm(Y0); #trick R CMD check
	W <- NULL; rm(W); #trick R CMD check
	H <- NULL; rm(H); #trick R CMD check

	fullname <- NULL; rm(fullname); #trick R CMD check
	.SD <- NULL; rm(.SD); #trick R CMD check
	level <- NULL; rm(level); #trick R CMD check
	color <- NULL; rm(color); #trick R CMD check
	ind <- NULL; rm(ind); #trick R CMD check
	clevel <- NULL; rm(clevel); #trick R CMD check

	for (i in 1:depth) {
		dats_i <- dats[[i]]
		if (i==1) {
			
			rec <- unlist(dats_i[1, list(X0, Y0, W, H)])
			value<-dats_i$value
			names(value) <- dats_i$index1
			recDat <- do.call(algorithm, list(value, rec))
			#recDat <- pivotSize(value, rec)
			recNames <- row.names(recDat)
			recDat <- as.data.table(recDat)
			recDat$ind <- factor(recNames, levels=levels(dats_i$index1))
			setkeyv(dats_i, "index1")
			setkeyv(recDat, "ind")
			dats_i <- cbind(dats_i, recDat[, list(x0, y0, w, h)])
		} else {
			indexList <- paste("index", 1:(i-1), sep="")
			indexList2 <- paste("index", 1:i, sep="")

			setkeyv(dats_i, indexList2)

			dats_i <- dats_i[dats[[i-1]][,
				c(indexList, "x0", "y0", "w", "h"), with=FALSE]]
			
			setnames(dats_i, c("x0", "y0", "w", "h"), c("X0", "Y0", "W", "H"))
			subTM <- function(x) {
				rec <- unlist((x[1, list(X0, Y0, W, H)]))
				x$index <- x[[paste("index", i, sep="")]]
				x <- x[order(x$sortInd),]
				value <- x$value
				names(value) <- x[[paste("index", i, sep="")]]
				
				recDat <- do.call(algorithm, list(value, rec))
				#recDat <- pivotSize(value, rec)
				recNames <- row.names(recDat)
				recDat <- as.data.table(recDat)
				recDat$ind <- factor(recNames, levels=levels(x$index))
				setkeyv(recDat, "ind")			
			}
			
			res <- dats_i[, subTM(.SD), by=indexList]
			setnames(res, "ind", paste("index", i, sep=""))
			setkeyv(res, indexList2)
			dats_i <- cbind(dats_i, res[, list(x0, y0, w, h)])
		}
		dats[[i]] <- dats_i
		dats_i$fullname <- do.call(paste, 
								   c(unclass(dats_i[, paste("index", 1:i, sep=""), 
								   			   with=FALSE]), sep="__"))
		setnames(dats_i, paste("index", i, sep=""), "ind")
		recList <- rbind(recList, dats_i[,
			list(ind, clevel, color, x0, y0, w, h, fullname)])
	}
	

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
		recs_fill <- createRec(recList, 
							   filled=TRUE, 
							   label="normal", 
							   labellb=lowerbound.cex.labels, 
							   lwd = 1,
							   inflate.labels=inflate.labels,
							   force.print.labels=force.print.labels,
							   cex_index=cex_indices[1])
		grid.draw(recs_fill$recs)
		grid.draw(recs_fill$txt)
	} else {

		whichBold <- recList$clevel==1
		lwds <- depth - recList$clevel + 1
		whichFill <- recList$clevel==depth
		
		recs_fill_norm <- createRec(recList[whichFill & !whichBold,], 
									filled=TRUE, 
									label="normal", 
									labellb=lowerbound.cex.labels, 
									lwd = lwds[whichFill & !whichBold], 
									inflate.labels=inflate.labels,
									force.print.labels=force.print.labels, 
									cex_index=cex_indices[3])
		
		recs_trans_norm <- createRec(recList[!whichFill & !whichBold,], 
									 filled=FALSE, 
									 label="normal",
									 labellb=lowerbound.cex.labels, 
									 lwd = lwds[!whichFill & !whichBold],
									 inflate.labels=inflate.labels,
									 force.print.labels=force.print.labels, 
									 cex_index=cex_indices[2]) 

		recs_trans_bold <- createRec(recList[!whichFill & whichBold,], 
									 filled=FALSE, 
									 label="bold", 
									 labellb=lowerbound.cex.labels, 
									 labelbg = TRUE, 
									 lwd = lwds[!whichFill & whichBold], 
									 inflate.labels=inflate.labels,
									 force.print.labels=force.print.labels, 
									 cex_index=cex_indices[1]) 
		
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
	
		drawRecs(recs_fill_norm)
		drawRecs(recs_trans_norm)
		drawRecs(recs_trans_bold)
		
	}
		
	
	## prepare output
	res <- strsplit(recList$fullname[whichFill], split="__")
	mat <- t(sapply(res, FUN=function(x){y <- rep("", depth)
								y[1:length(x)] <- x
								y}))
	
	mat[mat=="NA"] <- NA
	res <- as.data.frame(mat)
	names(res) <- indexNames
	resultDat <- cbind(res, as.data.frame(recList[whichFill, list(x0, y0, w, h)]))
	
	upViewport(3)
	#upViewport()
	return(resultDat)
}


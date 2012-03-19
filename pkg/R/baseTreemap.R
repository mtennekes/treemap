baseTreemap <-
function(dat,
	type,
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
	force.print.labels) {

	# determine available plot width and height
	width <- convertWidth(unit(1,"npc"),"inches",valueOnly = TRUE)
	height <- convertHeight(unit(1,"npc"),"inches",valueOnly = TRUE)
	
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
	  height = unit(1,"npc") - unit(1.5,"lines"),
	  gp=gpar(fontsize=fsData),
	  just = c("left", "bottom"))
	
	#determine depth
	depth <- sum(substr(colnames(dat),1,5)=="index")
#browser()
	dats <- list()

	datV <- data.table(value=numeric(0), value2=numeric(0))
	for (i in 1:depth) {
		indexList <- paste("index", 1:i, sep="")
		value <- NULL; rm(value)
		value2 <- NULL; rm(value2)
		sortInd <- NULL; rm(sortInd)
		#dats_i <- ddply(dat, indexList, colwise(sum, .(value, value2, sortInd)))

		dats_i <- dat[, lapply(.SD[, list(value, value2, sortInd)], sum), by=indexList]
		#setkeyv(dats_i, indexList)
		
		## remove rectangles of size 0
		dats_i <- dats_i[dats_i$value>0, ]

		if (type=="dens") {
			dats_i$value2 <- dats_i$value2 / dats_i$value
			dats_i$value2[is.nan(dats_i$value2)] <- 0
		}
		
		dats_i$clevel <- i
		dats_i <- dats_i[order(dats_i$sortInd),]
		
		datV <- rbind(datV, dats_i[, list(value, value2)])
		dats[[i]] <- dats_i
	}
	
	
	# Show legenda and determine colors
	if (legenda) {	
		pushViewport(vpLeg)
		grid.text(colorTitle, y = unit(0.5, "lines"))
		pushViewport(vpLeg2)
	}
	
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
		} else if (type == "value") {
			palette <- brewer.pal(11,"RdYlGn")
		}
	} else {
		if ((length(palette)==1) && (palette[1] %in%row.names(brewer.pal.info))) {
			palette <- brewer.pal(brewer.pal.info[palette, "maxcolors"], palette)
		}
	}
	
	if (type == "comp") {
		datV$color <- comp2col(datV, legenda, palette)
	} else if (type == "perc") {
		datV$color <- fill2col(datV, legenda, palette)
	} else if (type == "dens") {
		datV$color <- dens2col(datV, legenda, palette) 
	} else if (type == "linked") {
		datV$color <- fixed2col(datV, palette)
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
	grid.rect(name="TMrect")
		
	# Determine window size
	dataRec <- data.table(X0=0,Y0=0,
		W=convertWidth(unit(1, "grobwidth", "TMrect"),"inches",valueOnly=TRUE),
		H=convertHeight(unit(1, "grobheight", "TMrect"),"inches",valueOnly=TRUE))

	recList <- data.table(ind=character(0), 
						  clevel=numeric(0),
						  color=character(0),
						  x0=numeric(0),
						  y0=numeric(0),
						  w=numeric(0),
						   h=numeric(0))

	
	dats[[1]] <- cbind(dats[[1]], dataRec[rep(1,nrow(dats[[1]]))])	
	for (i in 1:depth) {
		dats_i <- dats[[i]]
		if (i==1) {
			rec <- unclass(dats_i[1, list(X0, Y0, W, H)])
			recDat <- pivotSize(dats_i, rec)
			recDat$ind <- factor(recDat$ind, labels=levels(dats_i$index1))
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
			res <- as.data.table(ddply(dats_i, indexList, function(x){
				rec <- unclass(x[1, list(X0, Y0, W, H)])
				x$index <- x[[paste("index", i, sep="")]]
				recDat <- pivotSize(x, rec)
				recDat$ind <- factor(recDat$ind, levels=1:nlevels(x$index), labels=levels(x$index))
				setkeyv(recDat, "ind")			
			}))
			setnames(res, "ind", paste("index", i, sep=""))
			setkeyv(res, indexList2)
			dats_i <- cbind(dats_i, res[, list(x0, y0, w, h)])
		}
		
		dats[[i]] <- dats_i
		setnames(dats_i, names(dats_i)[names(dats_i)==paste("index", i, sep="")], "ind")
		recList <- rbind(recList, dats_i[,
			list(ind, clevel, color, x0, y0, w, h)])
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
							   force.print.labels=force.print.labels)
		grid.draw(recs_fill$recs)
		grid.draw(recs_fill$txt)
	} else {
		whichBold <- recList$clevel==1
		lwds <- depth - recList$clevel + 1
		whichFill <- recList$clevel==depth
		
		recs_fill_bold <- createRec(recList[whichFill & whichBold,], 
									filled=TRUE, 
									label="bold", 
									labellb=lowerbound.cex.labels, 
									lwd = lwds[whichFill & whichBold],
									inflate.labels=inflate.labels,
									force.print.labels=force.print.labels)
		recs_fill_norm <- createRec(recList[whichFill & !whichBold,], 
									filled=TRUE, 
									label="normal", 
									labellb=lowerbound.cex.labels, 
									lwd = lwds[whichFill & !whichBold], 
									inflate.labels=inflate.labels,
									force.print.labels=force.print.labels)
		
		recs_trans_bold <- createRec(recList[!whichFill & whichBold,], 
									 filled=FALSE, 
									 label="bold", 
									 labellb=lowerbound.cex.labels, 
									 labelbg = TRUE, 
									 lwd = lwds[!whichFill & whichBold], 
									 inflate.labels=inflate.labels,
									 force.print.labels=force.print.labels) 
		recs_trans_norm <- createRec(recList[!whichFill & !whichBold,], 
									 filled=FALSE, 
									 label="normal",
									 labellb=lowerbound.cex.labels, 
									 lwd = lwds[!whichFill & !whichBold],
									 inflate.labels=inflate.labels,
									 force.print.labels=force.print.labels) 
		
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
		
		resultDat <- recList[whichFill, c("ind", "clevel", "x0", "y0", "w", "h")]

	upViewport()
	upViewport()
	return(resultDat)
}


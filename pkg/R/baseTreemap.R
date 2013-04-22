baseTreemap <-
function(dat,
	type,
	algorithm,
	position.legend,
	sizeTitle, 
	colorTitle,
	palette,
    range,
	fontsize.title, 
	fontsize.labels, 
	fontsize.legend, 
	lowerbound.cex.labels,
	inflate.labels,
	bg.labels,
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
	
	# Determine legend viewports
	titleSpace <- convertHeight(unit(1.5* (fsTitle/get.gpar()$fontsize),
									 "lines"), "inches")
	if (position.legend == "bottom") {
		legHeight <- unit(fsLegend * 0.03 + 0.4, "inches")
		legWidth <- unit(0, "npc")
		
		vpLeg <- viewport(name = "legenda",
						  x = plotMargin,
						  y = 0.5 * plotMargin + legHeight*0.3,
						  width = unit(1, "npc") - 2 * plotMargin,
						  height = legHeight*0.7,
						  gp=gpar(fontsize=fsLegend),
						  just = c("left", "bottom"))
		
		vpLeg2 <- viewport(name = "legenda_title",
						   x = plotMargin,
						   y = 0.5 * plotMargin,
						   width = unit(1, "npc") - 2 * plotMargin,
						   height = legHeight*0.3,
						   gp=gpar(fontsize=fsLegend),
						   just = c("left", "bottom"))
		
	} else if (position.legend == "right") {
		scale <- fsLegend / get.gpar()$fontsize
		maxStringWidth <- max(convertWidth(stringWidth(colorTitle), "inches",
										   valueOnly=TRUE)*scale+.5, 1)
		if (type=="categorical") {
			maxStringWidth	<- max(maxStringWidth, 
								  convertWidth(stringWidth(
								  	levels(dat$c)),
								  			 "inches",
								  			 valueOnly=TRUE)*scale+.75)
			
		} else if (type=="index") {
			maxStringWidth	<- max(maxStringWidth, 
								  convertWidth(stringWidth(
								  	indexNames),
								  			 "inches",
								  			 valueOnly=TRUE)*scale+.75)
		}

		legWidth <- unit(maxStringWidth, "inches")
		legHeight <- unit(0, "npc")
		vpLeg <- viewport(name = "legenda",
						  x = unit(1, "npc") - plotMargin - legWidth,
						  y = 0.5 * plotMargin,
						  width = legWidth,
						  height = unit(1, "npc") - plotMargin - titleSpace,
						  gp=gpar(fontsize=fsLegend),
						  just = c("left", "bottom"))
		
		vpLeg2 <- viewport(name = "legenda_title",
						   x = unit(1, "npc") - plotMargin - legWidth,
						   y = unit(1, "npc") - 0.5 * plotMargin - titleSpace,
						   width = legWidth,
						   height = titleSpace,
						   gp=gpar(fontsize=fsLegend),
						   just = c("left", "bottom"))
	} else {
		legWidth <- unit(0, "npc")
		legHeight <- unit(0, "npc")
	}
	
	vpDat <- viewport(name = "data",
					  x = plotMargin,
					  y = legHeight + 0.5*plotMargin,
					  width = unit(1, "npc") - 2 * plotMargin - legWidth,
					  height = unit(1,"npc") - 
					  	legHeight - plotMargin - titleSpace,
					  gp=gpar(fontsize=fsData),
					  just = c("left", "bottom"))
	
	vpDat2 <- viewport(name = "data_title", 
					   x = plotMargin,
					   y = unit(1, "npc") - .5*plotMargin - titleSpace,
					   width = unit(1, "npc") - 2 * plotMargin - legWidth,
					   height = titleSpace,
					   gp=gpar(fontsize=fsTitle),
					   just = c("left", "bottom"))
	
	#determine depth
	depth <- sum(substr(colnames(dat),1,5)=="index")
    
    indexList <- paste0("index", 1:depth)
    
    dats <- list()
    for (d in seq_len(depth)) {
        datd <- aggTmData(dat, indexList[1:d], na.rm=TRUE)
        if (d < depth) {
            indexPlus <- indexList[(d+1):depth]
            datd[, indexPlus] <- lapply(indexPlus, function(x)factor(NA))
            setcolorder(datd, c(indexList, "s", "c", "i"))
        }
        datd[, l:=d]
        datd <- datd[order(datd$i),]
        
        dats[[d]] <- datd
    }
        
    datlist <- do.call("rbind", dats)

	if (type=="dens") {
	    datlist$c <- datlist$c / datlist$s
	    datlist$c[is.nan(datlist$c)] <- 0
	}
	
    datlist$ind <- as.factor(do.call("paste", c(as.list(datlist[, c(indexList, "l"), with=FALSE]), sep="__")))
    
    
	# Show legenda and determine colors
	if (position.legend!="none") {	
		pushViewport(vpLeg2)
		grid.text(colorTitle)
		upViewport()
		pushViewport(vpLeg)
	}

    
    
    if (type == "comp") {
        datlist$color <- comp2col(datlist, position.legend, palette, range)
	} else if (type == "dens") {
	    datlist$color <- dens2col(datlist, position.legend, palette, range) 
	} else if (type == "linked") {
	    datlist$color <- linked2col(datlist, position.legend, palette)
	} else if (type == "depth") {
	    datlist$color <- depth2col(datlist, position.legend, palette, indexNames)
	} else if (type == "index") {
	    datlist$color <- index2col(datlist, position.legend, palette, levels(dat$index1))
	} else if (type == "value") {
	    datlist$color <- value2col(datlist, position.legend, palette, range)
	} else if (type == "categorical") {
	    datlist$color <- cat2col(datlist, position.legend, palette, levels(dat$c))
	}
	if (position.legend!="none") upViewport()
	
	pushViewport(vpDat2)
	grid.text(sizeTitle)
	upViewport()
	pushViewport(vpDat)
	
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
	
	vpDat3 <- viewport(name = "data_asp", 
					   x = unit(0.5, "npc"),
					   y = unit(0.5, "npc"),
					   width = unit(datWidth, "inches"),
					   height = unit(datHeight,"inches"),
					   gp=gpar(fontsize=fsData),
					   just = c("centre", "centre"))
	
	pushViewport(vpDat3)

	dataRec <- data.table(X0=0, Y0=0, W=datWidth, H=datHeight)

    
    datlist[, X0:=0]
	datlist[, Y0:=0]
	datlist[, W:=datWidth]
	datlist[, H:=datHeight]

	datlist[, x0:=0]
	datlist[, y0:=0]
	datlist[, w:=0]
	datlist[, h:=0]
    
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

	setkey(datlist, ind)

    
	subTM <- function(x) {
	    rec <- unlist((x[1, list(X0, Y0, W, H)]))
	    x <- x[order(x$i),]
	    value <- x$s
	    names(value) <- x$ind
	    
	    recDat <- do.call(algorithm, list(value, rec))
	    recNames <- row.names(recDat)
	    recDat <- as.data.table(recDat)
	    #recDat <- as.data.frame(recDat)
	    recDat$ind <- factor(recNames, levels=levels(x$ind))
	    setkeyv(recDat, "ind")    		
	}
	
	for (i in 1:depth) {
        if (i!=1) {
            active <- datlist$l==i
            parents_active <- datlist$l==(i-1)
            
            parents <- datlist[[paste0("index", i-1)]][active]
            parents2 <- datlist[[paste0("index", i-1)]][parents_active]
            matchID <- match(parents, parents2)
            
            datlist[active, c("X0", "X0", "W", "H"):= as.list(datlist[parents_active,][matchID, c("x0", "y0", "w", "h"), with=FALSE])]
            
            indList <- indexList[1:(i-1)]
            res <- datlist[active, subTM(.SD), by=indList]
        } else {
            res <- subTM(datlist[datlist$l==1])
        }
        setkey(res, ind)
        datlist[match(res$ind, datlist$ind), c("x0", "y0", "w", "h"):= as.list(res[, c("x0", "y0", "w", "h"), with=FALSE])]

    }
    
	browser()
	
    
    
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
									 labelbg = bg.labels, 
									 lwd = lwds[!whichFill & !whichBold],
									 inflate.labels=inflate.labels,
									 force.print.labels=force.print.labels, 
									 cex_index=cex_indices[2]) 

		recs_trans_bold <- createRec(recList[!whichFill & whichBold,], 
									 filled=FALSE, 
									 label="bold", 
									 labellb=lowerbound.cex.labels, 
									 labelbg = bg.labels, 
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
		#browser()
# 		if (!is.na(bg.labels)) {
# 			if (!is.na(recs_trans_norm$txtbg[1]))
# 				recs_trans_norm$txtbg$gp$fill <- NA
# 			if (!is.na(recs_trans_bold$txtbg[1]))
# 				recs_trans_bold$txtbg$gp$fill <- bg.labels
# 		}
	
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
	resultDat <- cbind(res, as.data.frame(recList[whichFill, 
												  list(x0, y0, w, h)]))
	
	upViewport(2)
	#upViewport()
	return(resultDat)
}


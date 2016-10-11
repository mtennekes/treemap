dens2col <-
function(dat, position.legend, palette, range, border.col, fontfamily.legend, n, format.legend) {
	color <- colorRampPalette(palette,space="rgb")(99)

    values <- dat$c
    
    if (any(is.na(range))) {
	    prettyP <- pretty(values,n=n)
    } else {
        prettyP <- pretty(range,n=n)
    }
	n <- length(prettyP)
	minP <- min(prettyP)
	maxP <- max(prettyP)
	if (maxP > 10000000) {
		prettyT <- paste(round(prettyP/1000000),"mln",sep="")
	} else if (maxP > 10000) {
		prettyT <- paste(round(prettyP/1000),"k",sep="")
	} else {
		prettyT <- format(prettyP, trim=TRUE)
	}
	
	legScale <- floor((prettyP - minP) / (maxP - minP) * 98) + 1
	legCol <- color[legScale]
	
	args.legend <- format.legend
	args.legend[["x"]] <- prettyT
	legendText <- do.call("format", args.legend)
	if (position.legend!="none") drawLegend(legendText, legCol,
											position.legend=="bottom", border.col, fontfamily.legend)

	scale <- floor((values - minP) / (maxP - minP) * 98) + 1
	if (any(scale<1)) {
	    warning("Values found that are lower than the minimum of range")
	    scale[scale<1] <- 1
	}
	if (any(scale>99)) {
	    warning("Values found that are higher than the maximum of range")
	    scale[scale>99] <- 99
	}
	return (list(color[scale], range(prettyP), values))
}


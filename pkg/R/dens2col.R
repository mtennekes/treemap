dens2col <-
function(dat, position.legend, palette, range) {
	color <- colorRampPalette(palette,space="rgb")(99)

    if (any(is.na(range))) {
	    prettyP <- pretty(dat$value2,n=8)
    } else {
        prettyP <- pretty(range,n=8)
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
	
	
	if (position.legend!="none") drawLegend(prettyT, legCol,
											position.legend=="bottom")

	scale <- floor((dat$value2 - minP) / (maxP - minP) * 98) + 1
	if (any(scale<1)) {
	    warning("Values found that are lower than the minimum of range")
	    scale[scale<1] <- 1
	}
	if (any(scale>99)) {
	    warning("Values found that are higher than the maximum of range")
	    scale[scale>99] <- 99
	}
	return (color[scale])
}


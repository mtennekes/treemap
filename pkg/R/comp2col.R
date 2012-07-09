comp2col <-
function(dat, position.legend, palette, range) {
	color <- colorRampPalette(palette,space="rgb")(99)
	
	
	perc <-((dat$value - dat$value2)/dat$value2) * 100
	
	growth <- (dat$value / dat$value2)
	
	# edit growth
	growth[growth<0.1] <- 0.1
	growth[growth>10] <- 10
	
	log.growth <- log(growth)
	
	# edit perc
	perc[perc==Inf] <- max(900, c(perc[perc!=Inf]))
	perc[perc==0] <- min(-90, c(perc[perc!=0]))
	
	range_lg <- range(log.growth)
	
    if (!any(is.na(range))) {
        if (range[1] < -100) range[1] <- -100
        prettyP <-pretty(range, n=8)
    } else if (range_lg[1] >= 0 || range_lg[2] <= 0) {
		prettyP <-pretty(perc, n=8)
	} else {
		ratio <- abs(range_lg) / sum(abs(range_lg))
		prettyP_neg <- pretty(c(0, perc[perc<0]), n=9*ratio[1])
		prettyP_pos <- pretty(c(0, perc[perc>0]), n=9*ratio[2])
		prettyP <- c(prettyP_neg, prettyP_pos[-1])
	}
    
    prettyP <- prettyP[prettyP > -100]
    
	n <- length(prettyP)	

	max_lg <- max(abs(log.growth))
	scale <- round(((log.growth/max_lg) *49)+50)
	
	perc2lg <- function(p) log((100+p)/100)
	
	
	legColScale <- perc2lg(prettyP)
	legColScale[legColScale < -max_lg] <- -max_lg
	legColScale[legColScale > max_lg] <- max_lg
	
	legCol <- color[round(((legColScale/max_lg) *49)+50)]
		
	if (position.legend!="none") drawLegend(paste(prettyP, "%", sep=""), legCol,
											position.legend=="bottom")
	return (color[scale])
}


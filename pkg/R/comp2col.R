comp2col <-
function(dat, position.legend, palette, range, border.col, fontfamily.legend, n, na.color, na.text, reverse.legend) {
    color <- colorRampPalette(palette,space="rgb")(99)
	perc <-((dat$s - dat$c)/dat$c) * 100
	
	growth <- (dat$s / dat$c)
	
	# edit growth
	growth[growth<0.1] <- 0.1
	growth[growth>10] <- 10
	
	log.growth <- log(growth)
	
	# edit perc
	perc.orig <- perc
	
    largeSign <- any(perc>1000) #to prevent legend scales > 1000
    perc[perc > 900] <- 900
 	perc[perc < -90] <- -90
	
	range_lg <- range(log.growth)
	
    if (!any(is.na(range))) {
        if (range[1] < -100) range[1] <- -100
        prettyP <-pretty(range, n=n)
    } else if (range_lg[1] >= 0 || range_lg[2] <= 0) {
		prettyP <-pretty(perc, n=n)
	} else {
		ratio <- abs(range_lg) / sum(abs(range_lg))
		prettyP_neg <- pretty(c(0, perc[perc<0]), n=n*ratio[1])
		prettyP_pos <- pretty(c(0, perc[perc>0]), n=n*ratio[2])
		prettyP <- c(prettyP_neg, prettyP_pos[-1])
	}
    
    prettyP <- prettyP[prettyP > -100]
    
    perc2lg <- function(p) log((100+p)/100)
    legColScale <- perc2lg(prettyP)
    
    if (!any(is.na(range))) {
        max_lg <- max(abs(legColScale))
    } else {
        max_lg <- max(abs(log.growth))
        if (max_lg==0) max_lg <- max(abs(legColScale))
    }
    
	scale <- round(((log.growth/max_lg) *49)+50)
    scale[scale<1] <- 1
    scale[scale>99] <- 99
    
	legColScale[legColScale < -max_lg] <- -max_lg
	legColScale[legColScale > max_lg] <- max_lg
	
	legCol <- color[round(((legColScale/max_lg) *49)+50)]
		
	if (position.legend!="none") {
        prettyString <- paste(prettyP, "%", sep="")
        if (largeSign) {
            lp <- prettyString[length(prettyString)]
            prettyString[length(prettyString)] <- paste(">", lp)
        }
        
        drawLegend(prettyString, legCol,
											position.legend=="bottom", border.col, fontfamily.legend, reverse.legend)
	}
	return (list(color[scale], range(prettyP), perc.orig))
}


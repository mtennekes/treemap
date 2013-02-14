index2col <-
function(dat, position.legend, palette, labels) {
    browser()
    
    ss <- strsplit(dat$value2, split="_")
    
    ss1 <- sapply(ss, function(x)x[1])
    ss2 <- sapply(ss, function(x)x[2])
    
    ss1 <- as.integer(ss1)
    ss2[ss2=="NA"] <- NA
    ss2 <- as.integer(ss2)
    
    
	color <- palette 
	colorl <- rep(color, length.out=max(dat$level))
		
	#if (position.legend!="none") drawLegend(labels, colorl, position.legend=="bottom")
	
	return (colorl[dat$level])
}
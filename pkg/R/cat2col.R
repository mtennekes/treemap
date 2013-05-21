cat2col <-
function(dat, position.legend, palette, labels) {
    
    l <- length(labels)
    
    if (palette[1]=="HCL") {
        require(colorspace)
        s <- spread(l)
        color <- hcl(seq(30, 390, length.out=l+1)[1:l], c=65, l=85)[s]
    } else {
        color <- palette
    }
    
	colorl <- rep(color, length.out=l)
	
	if (position.legend!="none") drawLegend(labels, colorl, position.legend=="bottom")
	
	return (colorl[dat$c])
}


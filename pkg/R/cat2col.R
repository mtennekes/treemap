cat2col <-
function(dat, position.legend, palette, labels) {
	color <- palette 
	colorl <- rep(color, length.out=length(labels))
	
	if (position.legend!="none") drawLegend(labels, colorl, position.legend=="bottom")
	
	return (colorl[dat$c])
}


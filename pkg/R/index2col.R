index2col <-
function(dat, position.legend, palette, indexnames) {
	color <- palette 
	colorl <- rep(color, length.out=max(dat$level))
		
	if (position.legend!="none") drawLegend(indexnames, colorl, position.legend=="bottom")
	
	return (colorl[dat$level])
}
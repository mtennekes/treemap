index2col <-
function(dat, palette) {
		color <- palette 
		colorl <- rep(color, length.out=max(dat$level))
		
		return (colorl[dat$level])
}
linked2col <-
function(dat, position.legend, palette) {
	
	color <- palette 

	sortID <- sort(as.character(dat$index),decreasing=FALSE,index.return=TRUE)$ix
	sortID2 <- sort(sortID, index.return=TRUE)$ix
	
	colorl <- rep(color, length.out=nrow(dat))
	return (colorl[sortID2])
}


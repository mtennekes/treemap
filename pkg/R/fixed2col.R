#' Creates colors for fixed treemaps
#'
#' Creates colors for fixed treemaps
#'
#' @param dat 
#' @return colorscale 
fixed2col <-
function(dat) {
	color <- c(brewer.pal(12,"Set3"),
		brewer.pal(8,"Set2")[c(1:4,7,8)],
		brewer.pal(9,"Pastel1")[c(1,2,4,5)])
	
	sortID <- sort(as.character(dat$index),decreasing=FALSE,index.return=TRUE)$ix
	sortID2 <- sort(sortID,index.return=TRUE)$ix
	
	colorl <- rep(color, length.out=nrow(dat))
	return (colorl[sortID2])
}


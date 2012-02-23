#' Creates colors and legenda for treemaps with percentages
#'
#' Creates colors and legenda for treemaps with percentages
#'
#' @param dat 
#' @param upperboundText
#' @param showScale
#' @param neg
#' @return colorscale 
fill2col <-
function(dat, upperboundText, showScale, neg) {
	color <- colorRampPalette(brewer.pal(9,"Blues"),space="rgb")(101)
	if (neg) {
		color <- color[101:1]
	}
	scale <- round(dat$value2)+1
	
	lineNpc <- convertHeight(unit(0.8,"lines"), "npc", valueOnly = TRUE)

	if (showScale) {	
		n <- 11
		# display legend 

		legX <- seq(0,1*((n-1)/n),length.out=n)
		legY <- rep(lineNpc,n)
	 	legW <- rep(1/n,n)
	 	legH <- rep(0.2,n)
		legScale <- seq(0,100,by=10)
	 	legCol <- color[legScale+1]
	 	leg <- data.frame(X=legX,Y=legY,W=legW,H=legH,Col=legCol)

		grid.rect(x=unit(leg$X,"npc"), y=unit(leg$Y,"npc"), width=unit(leg$W,"npc"), height=unit(leg$H,"npc"), 
			just=c("left","bottom"), gp = gpar(fill = as.character(leg$Col)))
		grid.text(paste(legScale,"%",sep=""),x=unit(leg$X+.5*leg$W, "npc"),y=unit(0, "npc") + 
			unit(0.5,"lines"),gp=gpar(cex=upperboundText*0.8))
	}
	return (color[scale])
}


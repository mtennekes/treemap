drawLegend <- function(labels, colors, bottom, border.col, fontfamily.legend, reverse.legend) {
	n <- length(labels)
	lineNpc <- convertHeight(unit(0.8,"lines"), "npc", valueOnly = TRUE)
	
	# display legend 
	
	rl = if (reverse.legend) rev else function(x) x
	
	if (bottom) {
		cellWidth <- max(convertWidth(stringWidth(labels), "inches", valueOnly=TRUE))
		totalWidth <- convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE)
		scale <-  min(1, (cellWidth * n) / totalWidth * 1.5)
		vpLeg_crop <- viewport(name = "legenda3",
						   width = scale,
						   height = 1)
		pushViewport(vpLeg_crop)
		
		
		legX <- seq(0,1*((n-1)/n), length.out=n)
		legY <- rep(lineNpc,n)
		legW <- rep(1/n,n)
		legH <- rep(0.2,n)
		leg <- data.frame(X=legX,Y=legY,W=legW,H=legH,Col=colors)
		
		grid.rect(x=unit(leg$X,"npc"), 
				  y=unit(leg$Y,"npc"), 
				  width=unit(leg$W,"npc"), 
				  height=unit(leg$H,"npc"), 
				  just=c("left","bottom"), 
				  gp = gpar(fill = rl(as.character(leg$Col)), col=border.col))
		grid.text(rl(labels),
				  x=unit(leg$X+.5*leg$W, "npc"),
				  y=unit(0, "npc") + unit(0.5,"lines"),
				  gp=gpar(cex=0.8, fontfamily=fontfamily.legend))	
		upViewport()
	} else {
		legX <- rep(0.05, n)
		legY <- 1-1.5*lineNpc-((0:(n-1))*(lineNpc*1.5))
		legW <- convertWidth(
			unit(max(0.2, convertWidth(unit(0.05, "npc"),
									   "inches", 
									   valueOnly=TRUE)), "inches"), "npc", valueOnly=TRUE)
		legH <- rep(lineNpc,n)
		leg <- data.frame(X=legX,Y=legY,W=legW,H=legH,Col=colors)
		
		grid.rect(x=unit(leg$X,"npc"), 
				  y=unit(leg$Y,"npc"), 
				  width=unit(leg$W,"npc"), 
				  height=unit(leg$H,"npc"), 
				  just=c("left","bottom"), 
				  gp = gpar(fill = rl(as.character(leg$Col)), col=border.col))
		grid.text(rl(labels),
				  x=unit(leg$X+leg$W+0.02, "npc"),
				  y=unit(leg$Y+.5*leg$H, "npc"),
				  gp=gpar(cex=1, fontfamily=fontfamily.legend), just=c("left", "center"))	
	}
}
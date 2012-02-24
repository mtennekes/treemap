dens2col <-
function(dat, showScale, palette) {
	color <- colorRampPalette(palette,space="rgb")(99)

	prettyP <- pretty(dat$value2,n=8)
	n <- length(prettyP)
	minP <- min(prettyP)
	maxP <- max(prettyP)
	if (maxP > 10000000) {
		prettyT <- paste(round(prettyP/1000000),"mln",sep="")
	} else if (maxP > 10000) {
		prettyT <- paste(round(prettyP/1000),"k",sep="")
	} else {
		prettyT <- prettyP
	}
	scale <- floor((dat$value2 - minP) / (maxP - minP) * 98) + 1

	lineNpc <- convertHeight(unit(0.8,"lines"), "npc", valueOnly = TRUE)
	
	if (showScale) {	
		# display legend 
		legX <- seq(0,1*((n-1)/n),length.out=n)
		legY <- rep(lineNpc,n)
	 	legW <- rep(1/n,n)
	 	legH <- rep(0.2,n)

		legScale <- floor((prettyP - minP) / (maxP - minP) * 98) + 1
	 	legCol <- color[legScale]
	 	leg <- data.frame(X=legX,Y=legY,W=legW,H=legH,Col=legCol)

		grid.rect(x=unit(leg$X,"npc"), y=unit(leg$Y,"npc"), width=unit(leg$W,"npc"), height=unit(leg$H,"npc"), 
			just=c("left","bottom"), gp = gpar(fill = as.character(leg$Col)))
		grid.text(prettyT,x=unit(leg$X+.5*leg$W, "npc"),y=unit(0, "npc") + unit(0.5,"lines"),
			gp=gpar(cex=0.8))
	}
	return (color[scale])
}


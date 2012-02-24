value2col <-
function(dat, showScale, palette, vColorRange) {

	if (any(is.na(vColorRange))) {
		vColorRange <- range(dat$value2)
		if (vColorRange[1] > 0) {
			## all positive
			vColorRange[1] <- 0
			prettyP <- pretty(vColorRange,n=8)
			palette <- palette[floor(length(palette)/2):length(palette)]
			
		} else if (vColorRange[2] < 0) {
			## all negative
			vColorRange[2] <- 0
			prettyP <- pretty(vColorRange,n=8)
			palette <- palette[1:ceiling(length(palette)/2)]
		} else {
			## positive and negative
			prettyP <- pretty(vColorRange,n=8)
			
			sumP <- sum(prettyP>0)
			sumN <- sum(prettyP<0)
			
			k <- max(sumP, sumN)
			
			colorTemp <- colorRampPalette(palette, space="rgb")(2*k+1)
			palette <- colorTemp[(k+1-sumN):(k+1+sumP)]
		}
		
	} else {
		prettyP <- pretty(vColorRange, n=8)
	}
	color <- colorRampPalette(palette, space="rgb")(99)
	n <- length(prettyP)
	legCol <- colorRampPalette(palette, space="rgb")(n)
	
	minP <- min(prettyP)
	maxP <- max(prettyP)

	scale <- floor((dat$value2 - minP) / (maxP - minP) * 98) + 1
	if (any(scale<1)) {
		warning("Values found that are lower than the minimum of vColorRange")
		scale[scale<1] <- 1
	}
	if (any(scale>99)) {
		warning("Values found that are higher than the maximum of vColorRange")
		scale[scale>99] <- 99
	}

	lineNpc <- convertHeight(unit(0.8,"lines"), "npc", valueOnly = TRUE)
	
	if (showScale) {	
		# display legend 
		legX <- seq(0,1*((n-1)/n),length.out=n)
		legY <- rep(lineNpc,n)
	 	legW <- rep(1/n,n)
	 	legH <- rep(0.2,n)

	 	leg <- data.frame(X=legX,Y=legY,W=legW,H=legH,Col=legCol)

		grid.rect(x=unit(leg$X,"npc"), y=unit(leg$Y,"npc"), width=unit(leg$W,"npc"), height=unit(leg$H,"npc"), 
			just=c("left","bottom"), gp = gpar(fill = as.character(leg$Col)))
		grid.text(prettyP, x=unit(leg$X+.5*leg$W, "npc"),y=unit(0, "npc") + unit(0.5,"lines"),
			gp=gpar(cex=0.8))
	}
	return (color[scale])
}

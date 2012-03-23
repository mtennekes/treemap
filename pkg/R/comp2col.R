comp2col <-
function(dat, showScale, palette) {
	color <- colorRampPalette(palette,space="rgb")(99)
	
	# calculate percentages
	# for legend
	perc <-((dat$value - dat$value2)/dat$value2) * 100
	
	# edit perc
	perc[perc==Inf] <- max(900, c(perc[perc!=Inf]))
	perc[perc==0] <- min(-90, c(perc[perc!=0]))
	
	
	perc2times <- function(p) {
		ifelse(p>=0, 
			   p/100,
			   -(1/(1-(abs(p)/100)) - 1))
	}
	
	times2log <- function(t) {
		res <- numeric(length(t))
		res[t>=0] <- log1p(t[t>=0])
		res[t<0] <- -log1p(-t[t<0])
		return(res)
	}
#browser()
	
	times <- times2log(perc2times(perc))
	range_times <- range(times)
	
	if (range_times[1] >= 0 || range_times[2] <= 0) {
		prettyP <-pretty(perc, n=8)
	} else {
		ratio <- abs(range_times) / sum(abs(range_times))
		prettyP_neg <- pretty(c(0, perc[perc<0]), n=9*ratio[1])
		prettyP_pos <- pretty(c(0, perc[perc>0]), n=9*ratio[2])
		prettyP <- c(prettyP_neg, prettyP_pos[-1])
	}
	
	n <- length(prettyP)	
	maxTimes <- max(abs(times))
	scale <- round(((times/maxTimes) *49)+50)
	
	lineNpc <- convertHeight(unit(0.8,"lines"), "npc", valueOnly = TRUE)

	if (showScale) {	
		# display legend 
		legX <- seq(0,1*((n-1)/n),length.out=n)
		legY <- rep(lineNpc,n)
	 	legW <- rep(1/n,n)
	 	legH <- rep(0.2,n)
		prettyT <- times2log(perc2times(prettyP))
		legScale <- round(((prettyT/maxTimes) *49)+50)
		legScale[legScale<1] <- 1
		legScale[legScale>99] <- 99
	 	legCol <- color[legScale]
	 	leg <- data.frame(X=legX,Y=legY,W=legW,H=legH,Col=legCol)
		
		grid.rect(x=unit(leg$X,"npc"), y=unit(leg$Y,"npc"), width=unit(leg$W,"npc"), 
			height=unit(leg$H,"npc"), just=c("left","bottom"), gp = gpar(fill = as.character(leg$Col)))
		grid.text(paste(prettyP,"%",sep=""),x=unit(leg$X+.5*leg$W, "npc"),y=unit(0, "npc") + unit(0.5,"lines"),
			gp=gpar(cex=0.8))
	}
	return (color[scale])
}


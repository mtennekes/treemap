# Determines whether one group of rectangles is overlapped by another group of rectangles
overlap <-
function(rec1, rec2, overlap.labels, select) {	
	#if (is.na(rec1[1])) return(NA)
	#if (is.na(rec2[1])) return(rep(FALSE, length(rec1$x)))
	
	x1 <- convertX(rec1$x, "npc", valueOnly=TRUE)
	y1 <- convertY(rec1$y, "npc", valueOnly=TRUE)

	widths1 <- convertWidth(rec1$width, "npc", valueOnly=TRUE)
	heights1 <- convertHeight(rec1$height, "npc", valueOnly=TRUE)
	xmin1 <- x1 - 0.5 * widths1
	xmax1 <- xmin1 + widths1
	ymin1 <- y1 - 0.5 * heights1
	ymax1 <- ymin1 + heights1
	
        
	widths2 <- convertWidth(rec2$width, "npc", valueOnly=TRUE)
	heights2 <- convertHeight(rec2$height, "npc", valueOnly=TRUE)
	xmin2 <- convertX(rec2$x, "npc", valueOnly=TRUE) - 0.5 * widths2
	xmax2 <- xmin2 + widths2
	ymin2 <- convertY(rec2$y, "npc", valueOnly=TRUE) - 0.5 * heights2
	ymax2 <- ymin2 + heights2

	results <- mapply(FUN=function(xmin1, xmax1, ymin1, ymax1){
        xoverlap <- 1 - pmin(pmax((xmax1 - xmax2) / (xmax1-xmin1), 0), 1) -
	        pmin(pmax((xmin2 - xmin1) / (xmax1-xmin1), 0), 1)
	    yoverlap <- 1 - pmin(pmax((ymax1 - ymax2) / (ymax1-ymin1), 0), 1) -
	        pmin(pmax((ymin2 - ymin1) / (ymax1-ymin1), 0), 1)
	    overlapping <- xoverlap * yoverlap
	    overlapping > overlap.labels
    }, xmin1, xmax1, ymin1, ymax1)
	
    
	if (is.vector(results)) results <- matrix(results, nrow=1)
    results <- results[select, , drop=FALSE]
	return(apply(results, MARGIN=2, FUN=any))
}


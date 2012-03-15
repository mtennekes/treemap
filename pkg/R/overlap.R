# Determines whether one group of rectangles is overlapped by another group of rectangles
overlap <-
function(rec1, rec2) {	
	if (is.na(rec1[1])) return(NA)
	if (is.na(rec2[1])) return(rep(FALSE, length(rec1$x)))
	
	x1 <- convertX(rec1$x, "npc", valueOnly=TRUE)
	y1 <- convertY(rec1$y, "npc", valueOnly=TRUE)

	
	widths2 <- convertWidth(rec2$width, "npc", valueOnly=TRUE)
	heights2 <- convertHeight(rec2$height, "npc", valueOnly=TRUE)
	xmin2 <- convertX(rec2$x, "npc", valueOnly=TRUE) - 0.5 * widths2
	xmax2 <- xmin2 + widths2
	ymin2 <- convertY(rec2$y, "npc", valueOnly=TRUE) - 0.5 * heights2
	ymax2 <- ymin2 + heights2

	results <- mapply(FUN=function(x1, y1, xmin2, xmax2, ymin2, ymax2){
		((y1 > ymin2) & (y1 < ymax2)) & ((x1 > xmin2) & (x1 < xmax2))},
					  x1, y1, MoreArgs=list(xmin2, xmax2, ymin2, ymax2))
	
	if (is.vector(results)) results <- matrix(results, nrow=1)
	
	return(apply(results, MARGIN=2, FUN=any))
}


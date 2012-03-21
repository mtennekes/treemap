squarified <-
function(value, rec){
	
	
	if (length(value)==1)
		return(matrix(rec, ncol=4, dimnames=list(names(value), 
												 c("x0", "y0", "w", "h"))))
	
	landscape <- rec[3] >= rec[4]
	
	value <- sort(value, decreasing=TRUE)
	tot <- sum(value)
	
	continue <- TRUE
	i <- 1
	bestAsp <- Inf
	
	while (continue) {
		totValue <- sum(value[1:i])
		totRatio <- totValue / tot
		minRatio <- min(value[1:i]) / totValue
		
		if (landscape) {
			totWidth <- totRatio * rec[3]
			minHeight <- rec[4] * minRatio
			asp <- max(totWidth/minHeight, minHeight/totWidth)
		} else {
			totHeight <- totRatio * rec[4]
			minWidth <- rec[3] * minRatio
			asp <- max(totHeight/minWidth, minWidth/totHeight)
		}
		
		if (asp < bestAsp && length(value)!=1) {
			bestAsp <- asp
			bestI <- i
			i <- i + 1
		} else {
			j <- i-1
			totValue <- sum(value[1:j])
			ratios <- value[1:j] / totValue
			if (landscape) {
				heights <- ratios * rec[4]
				recList <- matrix(c(rep(rec[1], j), 
								  rec[2] + c(0, cumsum(heights[1:j]))[1:j], 
								  rep(rec[3] * (totValue / tot), j),
								  heights), 
								  ncol=4, 
								  dimnames=list(names(value)[1:j], 
								  			  c("x0", "y0", "w", "h")))
				restRec <- c(rec[1] + rec[3] * (totValue / tot),
							 rec[2],
							 rec[3] * (1 - totValue / tot),
							 rec[4])
			} else {
				widths <- ratios * rec[3]
				recList <- matrix(c(rec[1] + c(0, cumsum(widths[1:j]))[1:j],
									rep(rec[2], j), 
									widths, 
									rep(rec[4] * (totValue / tot), j)),
								  ncol=4, 
								  dimnames=list(names(value)[1:j], 
								  			  c("x0", "y0", "w", "h")))
				restRec <- c(rec[1],
							 rec[2] + rec[4] * (totValue / tot),
							 rec[3],
							 rec[4] * (1 - totValue / tot))
			}
			if (j < length(value)) {
				recList <- rbind(recList,
								 squarified(value[i:length(value)], restRec))
			}
			continue <- FALSE
		}
	}
	return (recList)
}
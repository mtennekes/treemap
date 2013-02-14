index2col <-
function(dat, position.legend, palette, labels) {
    ss <- strsplit(dat$index, split="__")
    
    index1 <- as.integer(sapply(ss, function(x)x[1]))
    index2 <- sapply(ss, function(x)x[2])
    
    index2[index2=="NA"] <- NA
    index2 <- as.integer(index2)
    
    indexDT <- data.table(index1=index1, index2=index2)
    
    
    condense <- function(x) {
        y <- order(order(x))
        y[is.na(x)] <- NA
        y
    }
    
    indexDT <- indexDT[, list(index2=condense(index2)), by=index1]

    
    color <- palette 
    colorl <- rep(color, length.out=max(index1))
    
    indexDT$base_color <- colorl[indexDT$index1]
	
    browser()
    
	if (position.legend!="none") drawLegend(labels, colorl, position.legend=="bottom")
	
	return (indexDT$base_color)
}
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
    indexDT[, index2:=condense(index2), by=index1]

    
    color <- palette 
    colorl <- rep(color, length.out=max(index1))
    
    indexDT[, base_color:=colorl[indexDT$index1]]
	
    
    spread <- function(x) {
        y <- rep(1, length(x))
        if (!all(is.na(x))) {
            y[!is.na(x)] <- seq(0.5, 1.25, length.out=sum(!is.na(x)))[order(order(na.omit(x)))]
        }
        y
    }
    
    indexDT[, fact:=spread(index2), by=index1]
    
    require(colorspace)
    createColor <- function(bc, f) {
        co <- coords(as(hex2RGB(bc), "HSV"))
        co[, "V"] <- co[, "V"] * f
        co[, "V"][co[, "V"]>1] <- 1
        hex(HSV(co))
    }
    
    
    indexDT[, color:=createColor(base_color, fact)]
    
    
	if (position.legend!="none") drawLegend(labels, colorl, position.legend=="bottom")
	
	return (indexDT$color)
}
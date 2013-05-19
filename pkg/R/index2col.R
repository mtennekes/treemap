index2col <-
function(dat, position.legend, palette, labels) {
    require(colorspace)
    
    
    
    
    #browser()
    #dt <- copy(dat)
    
    depth <- sum(substr(names(dat), 1, 5)=="index")
    
    #dt[, md:=apply(dt[, 1:depth, with=FALSE], MARGIN=1, FUN=function(x)sum(!is.na(x)))]

    
    #indexList <- paste0("index", 1:depth)
    
    
    dat$color <- if (palette[1]=="HCL") {
        treepalette(dat[,1:depth,with=FALSE], method="HCL", frc=.5)
    } else {
        treepalette(dat[,1:depth,with=FALSE], method="HSV", palette=palette)       
    }

     
 	if (position.legend!="none") {
 	    labels <- dat$n[dat$l==1]
  	    colorl <- dat$color[dat$l==1]
 	    drawLegend(labels, colorl, position.legend=="bottom")
 	}
    
	
	return (dat$color)
}
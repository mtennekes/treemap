index2col <-
function(dat, position.legend, palette, labels, palette.HCL.options, border.col, fontfamily.legend, reverse.legend) {
    #require(colorspace)
    
    
    
    depth <- sum(substr(names(dat), 1, 5)=="index")
    
    #dt[, md:=apply(dt[, 1:depth, with=FALSE], MARGIN=1, FUN=function(x)sum(!is.na(x)))]
    dat$color <- if (palette[1]=="HCL") {
        treepalette(dat[,1:depth,with=FALSE], method="HCL", palette.HCL.options=palette.HCL.options, prepare.dat=FALSE, return.parameters=FALSE)
    } else {
        treepalette(dat[,1:depth,with=FALSE], method="HSV", palette=palette, prepare.dat=FALSE, return.parameters=FALSE)       
    }

     
 	if (position.legend!="none") {
 	    labels <- dat$n[dat$l==1]
  	    colorl <- dat$color[dat$l==1]
 	    drawLegend(labels, colorl, position.legend=="bottom", border.col, fontfamily.legend, reverse.legend)
 	}
    
	
	return (dat$color)
}
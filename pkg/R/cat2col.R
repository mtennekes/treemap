cat2col <-
function(dat, position.legend, palette, labels, palette.HCL.options, border.col, na.color, na.text, fontfamily.legend, reverse.legend) {
    
    l <- length(labels)
    
    if (palette[1]=="HCL") {
        #require(colorspace)
        s <- spread(l)
        color <- hcl(seq(palette.HCL.options$hue_start, palette.HCL.options$hue_end, length.out=l+1)[1:l], c=palette.HCL.options$chroma, l=palette.HCL.options$luminance)[s]
    } else {
        color <- palette
    }
    
	colorl <- rep(color, length.out=l)
	
	if (position.legend!="none") drawLegend(labels, colorl, position.legend=="bottom", border.col, fontfamily.legend, reverse.legend)
	
	return (list(colorl[dat$c], NA, dat$c))
}


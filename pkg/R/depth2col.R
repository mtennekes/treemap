depth2col <-
function(dat, position.legend, palette, indexNames, palette.HCL.options, border.col, fontfamily.legend, reverse.legend) {
    
    depth <- length(indexNames)

    if (palette[1]=="HCL") {
        #require(colorspace)
        s <- spread(depth)
        color <- hcl(seq(palette.HCL.options$hue_start, palette.HCL.options$hue_end, length.out=depth+1)[1:depth], c=palette.HCL.options$chroma, l=palette.HCL.options$luminance)[s]
    } else {
        color <- palette
    }
    
    
    depthID <- apply(dat[, 1:depth, with=FALSE], MARGIN=1, FUN=function(x)sum(!is.na(x)))

    colorl <- rep(color, length.out=depth)
    
    if (position.legend!="none") drawLegend(indexNames, colorl, position.legend=="bottom", border.col, fontfamily.legend, reverse.legend)
    
    return (colorl[depthID])
}


depth2col <-
function(dat, position.legend, palette, indexNames) {
    
    depth <- length(indexNames)

    if (palette=="HCL") {
        require(colorspace)
        s <- spread(depth)
        color <- hcl(seq(30, 390, length.out=depth+1)[1:depth], c=65, l=85)[s]
    } else {
        color <- palette
    }
    
    
    depthID <- apply(dat[, 1:depth, with=FALSE], MARGIN=1, FUN=function(x)sum(!is.na(x)))

    colorl <- rep(color, length.out=depth)
    
    if (position.legend!="none") drawLegend(indexNames, colorl, position.legend=="bottom")
    
    return (colorl[depthID])
}


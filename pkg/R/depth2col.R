depth2col <-
function(dat, position.legend, palette, indexNames) {
    color <- palette
    
    depth <- length(indexNames)
    depthID <- apply(dat[, 1:depth, with=FALSE], MARGIN=1, FUN=function(x)sum(!is.na(x)))

    colorl <- rep(color, length.out=depth)
    
    if (position.legend!="none") drawLegend(indexNames, colorl, position.legend=="bottom")
    
    return (colorl[depthID])
}


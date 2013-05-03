tmColorsLegend <- function(datlist, vps, position.legend, type, palette, range, indexNames) {
    if (position.legend!="none") {    
        pushViewport(vps$vpLeg)
    }
    
    if (type == "comp") {
        datlist$color <- comp2col(datlist, position.legend, palette, range)
    } else if (type == "dens") {
        datlist$color <- dens2col(datlist, position.legend, palette, range) 
    } else if (type == "depth") {
        datlist$color <- depth2col(datlist, position.legend, palette, indexNames)
    } else if (type == "index") {
        datlist$color <- index2col(datlist, position.legend, palette, levels(datlist$index1))
    } else if (type == "value") {
        datlist$color <- value2col(datlist, position.legend, palette, range)
    } else if (type == "categorical") {
        datlist$color <- cat2col(datlist, position.legend, palette, levels(datlist$c))
    }
    if (position.legend!="none") upViewport()
    
    datlist
}
tmColorsLegend <- function(datlist, vps, position.legend, type, palette, range, indexNames, palette.HCL.options, border.col, fontfamily.legend) {
    if (position.legend!="none") {    
        pushViewport(vps$vpLeg)
    }
    
    if (type == "comp") {
        datlist$color <- comp2col(datlist, position.legend, palette, range, border.col, fontfamily.legend)
    } else if (type == "dens") {
        datlist$color <- dens2col(datlist, position.legend, palette, range, border.col, fontfamily.legend) 
    } else if (type == "depth") {
        datlist$color <- depth2col(datlist, position.legend, palette, indexNames, palette.HCL.options, border.col, fontfamily.legend)
    } else if (type == "index") {
        datlist$color <- index2col(datlist, position.legend, palette, levels(datlist$index1), palette.HCL.options, border.col, fontfamily.legend)
    } else if (type == "value") {
        datlist$color <- value2col(datlist, position.legend, palette, range, border.col, fontfamily.legend)
    } else if (type == "categorical") {
        datlist$color <- cat2col(datlist, position.legend, palette, levels(datlist$c), palette.HCL.options, border.col, fontfamily.legend)
    }
    if (position.legend!="none") upViewport()
    
    datlist
}
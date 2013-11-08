tmColorsLegend <- function(datlist, vps, position.legend, type, palette, range, indexNames, palette.HCL.options, border.col, fontfamily.legend) {
    if (position.legend!="none") {    
        pushViewport(vps$vpLeg)
    }
    
    res <- if (type == "comp") {
        comp2col(datlist, position.legend, palette, range, border.col, fontfamily.legend)
    } else if (type == "dens") {
        dens2col(datlist, position.legend, palette, range, border.col, fontfamily.legend) 
    } else if (type == "depth") {
        depth2col(datlist, position.legend, palette, indexNames, palette.HCL.options, border.col, fontfamily.legend)
    } else if (type == "index") {
        index2col(datlist, position.legend, palette, levels(datlist$index1), palette.HCL.options, border.col, fontfamily.legend)
    } else if (type == "value") {
        value2col(datlist, position.legend, palette, range, border.col, fontfamily.legend)
    } else if (type == "categorical") {
        cat2col(datlist, position.legend, palette, levels(datlist$c), palette.HCL.options, border.col, fontfamily.legend)
    }

    if (type %in% c("comp", "dens", "value")) {
        datlist$color <- res[[1]]
        range <- res[[2]]
    } else {
        #datlist[, color:=res]
        datlist$color <- res
        range <- NA
    }
    
    if (position.legend!="none") upViewport()
    assign("range", range, envir=parent.frame()) # trick to prevent internal data.table copy on return
    datlist
}
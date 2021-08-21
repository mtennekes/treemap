tmColorsLegend <- function(datlist, vps, position.legend, type, palette, range, mapping, indexNames, palette.HCL.options, border.col, fontfamily.legend, n, na.color, na.text, format.legend, reverse.legend) {
    if (position.legend!="none") {    
        pushViewport(vps$vpLeg)
    }
    
    res <- if (type == "comp") {
        comp2col(datlist, position.legend, palette, range, border.col, fontfamily.legend, n=n, na.color, na.text, reverse.legend = reverse.legend)
    } else if (type == "dens") {
        dens2col(datlist, position.legend, palette, range, border.col, fontfamily.legend, n=n, na.color, na.text, format.legend = format.legend, reverse.legend = reverse.legend) 
    } else if (type == "depth") {
        depth2col(datlist, position.legend, palette, indexNames, palette.HCL.options, border.col, fontfamily.legend, reverse.legend = reverse.legend)
    } else if (type == "index") {
        index2col(datlist, position.legend, palette, levels(datlist$index1), palette.HCL.options, border.col, fontfamily.legend, reverse.legend = reverse.legend)
    } else if (type == "value") {
        value2col(datlist, position.legend, palette, range, mapping, border.col, fontfamily.legend, auto.col.mapping=TRUE, n=n, na.color, na.text, format.legend = format.legend, reverse.legend = reverse.legend)
    } else if (type == "manual") {
        value2col(datlist, position.legend, palette, range, mapping, border.col, fontfamily.legend, auto.col.mapping=FALSE, n=n, na.color, na.text, format.legend = format.legend, reverse.legend = reverse.legend)
    } else if (type == "categorical") {
        cat2col(datlist, position.legend, palette, levels(datlist$c), palette.HCL.options, border.col, na.color, na.text, fontfamily.legend, reverse.legend = reverse.legend)
    }

    if (type %in% c("comp", "dens", "value", "manual", "categorical")) {
        datlist$color <- res[[1]]
        range <- res[[2]]
        datlist$colorvalue <- res[[3]]
    } else {
        #datlist[, color:=res]
        datlist$color <- res
        range <- NA
        datlist$colorvalue <- NA
    }
    
    if (position.legend!="none") upViewport()
    assign("range", range, envir=parent.frame()) # trick to prevent internal data.table copy on return
    
    datlist$color[is.na(datlist$color)] <- na.color
    
    datlist
}
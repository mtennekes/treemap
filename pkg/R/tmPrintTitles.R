tmPrintTitles <- function(vps, title, title.legend, position.legend, fontfamily.title, fontfamily.legend) {
    pushViewport(vps$vpDatTitle)
    grid.text(title, gp=gpar(fontfamily=fontfamily.title))
    upViewport()
    
    if (position.legend!="none") {    
        pushViewport(vps$vpLegTitle)
        grid.text(title.legend, gp=gpar(fontfamily=fontfamily.legend))
        upViewport()
    }
}
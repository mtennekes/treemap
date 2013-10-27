tmPrintTitles <- function(vps, title, title.legend, position.legend, font.title, font.legend) {
    pushViewport(vps$vpDatTitle)
    grid.text(title, gp=gpar(fontfamily=font.title))
    upViewport()
    
    if (position.legend!="none") {    
        pushViewport(vps$vpLegTitle)
        grid.text(title.legend, gp=gpar(fontfamily=font.legend))
        upViewport()
    }
}
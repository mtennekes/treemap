tmPrintTitles <- function(vps, title, title.legend, position.legend) {
    pushViewport(vps$vpDatTitle)
    grid.text(title)
    upViewport()
    
    if (position.legend!="none") {    
        pushViewport(vps$vpLegTitle)
        grid.text(title.legend)
        upViewport()
    }
}
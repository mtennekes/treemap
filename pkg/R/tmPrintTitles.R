tmPrintTitles <- function(vps, title, subtitle, position.legend) {
    pushViewport(vps$vpDatTitle)
    grid.text(title)
    upViewport()
    
    if (position.legend!="none") {    
        pushViewport(vps$vpLegTitle)
        grid.text(subtitle)
        upViewport()
    }
}
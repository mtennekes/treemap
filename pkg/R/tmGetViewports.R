tmGetViewports <- function(vp, fontsize.title, fontsize.labels, fontsize.legend,
                              position.legend, type, aspRatio, title.legend, catLabels) {

    ############
    ## Prepare plot viewport
    ############
    if (is.null(vp)) {
        grid.newpage()
    } else {
        if (is.character(vp)) 
            seekViewport(vp)
        else pushViewport(vp)
    }    
    
    width <- convertWidth(unit(1,"npc"), "inches",valueOnly = TRUE)
    height <- convertHeight(unit(1,"npc"), "inches",valueOnly = TRUE)
    
    plotMargin <- unit(0.5,"cm")
    
    # determine fontsizes
    fsTitle <- fontsize.title#min(fontsize.title, (height*3.6), (width*3.6))
    fsData <- 12 #max(fontsize.labels)
    #fsData <- max(min(fontsize.labels), 1)#min(fontsize.labels, (height*3.6), (width*3.6))
    fsLegend <- fontsize.legend#min(fontsize.legend, (height*3.6), (width*3.6))
    
    # Determine legend viewports
    titleSpace <- convertHeight(unit(1.5* (fsTitle/get.gpar()$fontsize),
                                     "lines"), "inches")
    if (position.legend == "bottom") {
        legHeight <- unit(fsLegend * 0.03 + 0.4, "inches")
        legWidth <- unit(0, "npc")
        
        vpLeg <- viewport(name = "legenda",
                          x = plotMargin,
                          y = 0.5 * plotMargin + legHeight*0.3,
                          width = unit(1, "npc") - 2 * plotMargin,
                          height = legHeight*0.7,
                          gp=gpar(fontsize=fsLegend),
                          just = c("left", "bottom"))
        
        vpLegTitle <- viewport(name = "legenda_title",
                           x = plotMargin,
                           y = 0.5 * plotMargin,
                           width = unit(1, "npc") - 2 * plotMargin,
                           height = legHeight*0.3,
                           gp=gpar(fontsize=fsLegend),
                           just = c("left", "bottom"))
        
    } else if (position.legend == "right") {
        scale <- fsLegend / get.gpar()$fontsize
        maxStringWidth <- max(convertWidth(stringWidth(title.legend), "inches",
                                           valueOnly=TRUE)*scale+.5, 1)
        if (type %in% c("categorical", "index")) {
            maxStringWidth	<- max(maxStringWidth, 
                                  convertWidth(stringWidth(catLabels),
                                               "inches",
                                               valueOnly=TRUE)*scale+.75)
            
        }
        
        legWidth <- unit(maxStringWidth, "inches")
        legHeight <- unit(0, "npc")
        vpLeg <- viewport(name = "legenda",
                          x = unit(1, "npc") - plotMargin - legWidth,
                          y = 0.5 * plotMargin,
                          width = legWidth,
                          height = unit(1, "npc") - plotMargin - titleSpace,
                          gp=gpar(fontsize=fsLegend),
                          just = c("left", "bottom"))
        
        vpLegTitle <- viewport(name = "legenda_title",
                           x = unit(1, "npc") - plotMargin - legWidth,
                           y = unit(1, "npc") - 0.5 * plotMargin - titleSpace,
                           width = legWidth,
                           height = titleSpace,
                           gp=gpar(fontsize=fsLegend),
                           just = c("left", "bottom"))
    } else {
        legWidth <- unit(0, "npc")
        legHeight <- unit(0, "npc")
        vpLeg <- NA
        vpLegTitle <- NA
    }
    
    vpDat <- viewport(name = "data",
                      x = plotMargin,
                      y = legHeight + 0.5*plotMargin,
                      width = unit(1, "npc") - 2 * plotMargin - legWidth,
                      height = unit(1,"npc") - 
                          legHeight - plotMargin - titleSpace,
                      gp=gpar(fontsize=fsData),
                      just = c("left", "bottom"))
    
    vpDatTitle <- viewport(name = "data_title", 
                       x = plotMargin,
                       y = unit(1, "npc") - .5*plotMargin - titleSpace,
                       width = unit(1, "npc") - 2 * plotMargin - legWidth,
                       height = titleSpace,
                       gp=gpar(fontsize=fsTitle),
                       just = c("left", "bottom"))
    
    pushViewport(vpDat)
    datWidth <- convertWidth(unit(1,"npc"), "inches", valueOnly=TRUE)
    datHeight <- convertHeight(unit(1,"npc"), "inches", valueOnly=TRUE)
    aspWindow <- datWidth / datHeight
    
    if (!is.na(aspRatio)) {
        if (aspRatio < aspWindow) {
            datWidth <- datHeight * aspRatio
        } else if (aspRatio > aspWindow) {
            datHeight <- datWidth / aspRatio
        }
    } else {
        aspRatio <- datWidth / datHeight
    }
    
    vpDatAsp <- viewport(name = "data_asp", 
                       x = unit(0.5, "npc"),
                       y = unit(0.5, "npc"),
                       width = unit(datWidth, "inches"),
                       height = unit(datHeight,"inches"),
                       gp=gpar(fontsize=fsData),
                       just = c("centre", "centre"))
    upViewport()
    vpCoorY <- ((convertY(vpDat$y, unitTo="inch", valueOnly=TRUE) + 
                    convertHeight(vpDat$height, unitTo="inch", valueOnly=TRUE)/2)  + c(-1, 1) * (datHeight/2)) / height
    vpCoorX <- ((convertX(vpDat$x, unitTo="inch", valueOnly=TRUE) + 
                     convertWidth(vpDat$width, unitTo="inch", valueOnly=TRUE)/2)  + c(-1, 1) * (datWidth/2)) / width
    
    list(vpDat=vpDat, vpDatAsp=vpDatAsp, vpDatTitle=vpDatTitle, vpLeg=vpLeg, vpLegTitle=vpLegTitle, datWidth=datWidth, datHeight=datHeight, aspRatio=aspRatio, vpCoorX=vpCoorX, vpCoorY=vpCoorY)
}
#' Return info about clicked rectangle
#'
#' Return information about the rectangle the user clicked on. Useful to identify small rectangles in large treemaps. 
#' @param coor mouse click in npc coordinates
#' @param tmSave treemap information: value returned by \code{\link{treemap}}
#' @import grid
tmLocate <-
    function(coor, tmSave) {
        tm <- tmSave$tm
        
#         # convert click to inches
#         inchClick <- list()
#         inchClick$x <- convertX(unit(npcClick$x, "npc"), "inches")
#         inchClick$y <- convertY(unit(npcClick$y, "npc"), "inches")
#         
#         # zoom in to selected treemap
#         downViewport("data")
#         downViewport("data_asp")
#         
#         # determine treemap size in inches
#         sizeTm <- c(convertWidth(unit(1, "npc"), "inches", valueOnly=TRUE),
#                     convertHeight(unit(1, "npc"), "inches", valueOnly=TRUE))
#         
#         # get transformation matrix
#         trans <- current.transform()
#         
#         # go to root viewport
#         upViewport(n=2)
#         
#         # calculate click (inches) relative to selected treemap
#         tempCoor <- c(unlist(inchClick), 1) %*% solve(trans)
#         inchCoor <- (tempCoor/tempCoor[3])[-3]
        
        # transform to npc coordinates
#        coor <- unlist(npcClick) #inchCoor / sizeTm
        
        #cat("click: x y", coor[1], coor[2], "\n")
        
        # retrieve selected rectangle
        rectInd <- which(tm$x0 < coor[1] &
                             (tm$x0 + tm$w) > coor[1] &
                             tm$y0 < coor[2] &
                             (tm$y0 + tm$h) > coor[2])
        
#         if (length(rectInd)==0) {
#             stop("no treemap selected")
#         }
        
        return(tm[rectInd[1], ])
    }
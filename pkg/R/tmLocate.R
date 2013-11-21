#' Return info about clicked rectangle
#'
#' Return information about the rectangle the user clicked on. Useful to identify small rectangles in large treemaps. 
#' @param coor mouse click in npc coordinates
#' @param tmSave treemap information: value returned by \code{\link{treemap}}
#' @import grid
tmLocate <-
    function(coor, tmSave) {
        tm <- tmSave$tm
        
        # retrieve selected rectangle
        rectInd <- which(tm$x0 < coor[1] &
                             (tm$x0 + tm$w) > coor[1] &
                             tm$y0 < coor[2] &
                             (tm$y0 + tm$h) > coor[2])
        
        return(tm[rectInd[1], ])
    }
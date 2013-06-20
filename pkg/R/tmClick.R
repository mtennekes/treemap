#' Mouse capture
#'
#' Capture a mouse click and convert it to npc coordinates. These coordinates are required for \code{\link{tmLocate}}, a function to identify rectangles. 
#'
#' @import grid
#' @export
tmClick <-
    function() {
        # get mouse click (in relative coordinates (from 0 to 1))
        click_window <- grid.locator("npc")
        x <- as.numeric(click_window$x)
        y <- as.numeric(click_window$y)
        return(list(x=x, y=y))
    }
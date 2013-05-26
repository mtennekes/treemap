#' Create a treemap (deprecated)
#'
#' This function is migrated to \code{\link{treemap}}.
#' 
#' @param ... passed on to \code{\link{treemap}}
#' @export
tmPlot <- function(...) {
    cat("Note: tmPlot deprecated as of version 2.0. Please use treemap instead.\n")
    invisible(treemap(...))
}
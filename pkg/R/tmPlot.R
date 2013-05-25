#' Create a treemap (deprecated)
#'
#' This function is migrated to \code{\link{treemap}}.
#' 
#' @param ... passed on to \code{\link{treemap}}
#' @export
tmPlot <- function(...) {
    invisible(treemap(...))
}
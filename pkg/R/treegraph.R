#' Tree graph
#'
#' This function draws a tree graph with a radial layout. Experimental.
#' 
#' @param dtf a data.frame or data.table. Required.
#' @param index the index variables of dtf (see \code{\link{treemap}})
#' @param palette.HCL.options palette.HCL.options (see \code{\link{treemap}})
#' @param show.labels show the labels
#' @param rootlabel name of the rootlabel
#' @param vertex.layout layout algorithm
#' @param vertex.layout.params list of arguments passed to \code{vertex.layout}
#' @param truncate.labels number of characters at which the levels are truncated. Either a single value for all index variables, or a vector of values for each index variable
#' @param vertex.size vertex.size (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param vertex.label.dist vertex.label.dist (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param vertex.label.cex vertex.label.cex (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param vertex.label.family vertex.label.family (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param vertex.label.color vertex.label.color (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param ... arguments passed to \code{\link[igraph:plot.igraph]{plot.igraph}}
#' @return (invisible) igraph object
#' @examples
#' data(business)
#' treegraph(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), show.labels=FALSE)
#' treegraph(business[business$NACE1=="F - Construction",], 
#'     index=c("NACE2", "NACE3", "NACE4"), show.labels=TRUE, truncate=c(2,4,6))
#' @import data.table
#' @import igraph
#' @import colorspace
#' @export
treegraph <- function(dtf, index=names(dtf), palette.HCL.options, show.labels=FALSE, rootlabel="", vertex.layout, vertex.layout.params, truncate.labels=NULL, vertex.size=3, vertex.label.dist=0.3, vertex.label.cex=0.8, vertex.label.family="sans", vertex.label.color="black", ...) {
    palette.HCL.options <- tmSetHCLoptions(palette.HCL.options)
    
    k <- length(index)
    dat <- treepalette(dtf, index,
                       palette.HCL.options=palette.HCL.options,
                       return.parameters=TRUE)[, 1:(k+1)]
    
    dat <- unique(dat)
    
    if (!missing(truncate.labels)) {
        truncate.labels <- rep(truncate.labels, length.out=k)
        for (i in 1:k) {
            levels(dat[[i]]) <- substr(levels(dat[[i]]), 1, truncate.labels[i])
        }
    }
    
    dat <- cbind(dat, as.data.table(treeid(dat[,1:k, drop=FALSE])))
    vdat <- dat[,c("current", "HCL.color")]
    setnames(vdat, "HCL.color", "color")
    vdat <- unique(vdat)
    rootname <- dat$parent[which(substr(dat$parent, 1, 2)=="NA")][1]
    vdat <- rbind(list(current=rootname, color=hcl(h=0, c=0, l=palette.HCL.options$luminance-palette.HCL.options$luminance_slope)), vdat)
    
    
    
    g <- graph.data.frame(dat[,c("current", "parent")], vertices=vdat, directed=FALSE)
    
    if (show.labels) {
        
        vdat_names <- gsub("__NA", "", vdat$current, fixed=TRUE)
        vdat_names[1] <- rootlabel
        
        ssres <- strsplit(vdat_names, split="__", fixed=TRUE)
        ssres[[1]] <- rootlabel
        vdat_names <- sapply(ssres, function(x)x[length(x)])
        
        ## equal string lengths
        lengths <- nchar(vdat_names)
        heads <- floor((max(lengths)-lengths)/2)
        tails <- max(lengths) - lengths - heads
        hs <- sapply(heads, function(x)paste(rep(" ", x), collapse=""))
        ts <- sapply(tails, function(x)paste(rep(" ", x), collapse=""))
        vdat_names <- mapply(paste0, hs, vdat_names, ts)
    } else {
        vdat_names <- NA
    }
    
    
    par(mai=c(0,0,0,0))
    if (missing(vertex.layout)){
        vertex.layout=layout.reingold.tilford
    }
    if (missing(vertex.layout.params)){
        vertex.layout.params = list(circular=T, root=1)
    }
    vertex.layout.params = c(list(graph=g), vertex.layout.params)
    vertex.layout = do.call(vertex.layout, vertex.layout.params)
    plot(g, vertex.size=vertex.size, vertex.frame.color=NA, vertex.label=vdat_names, layout=vertex.layout
         , vertex.label.dist=vertex.label.dist, vertex.label.cex=vertex.label.cex, vertex.label.family=vertex.label.family, vertex.label.color=vertex.label.color, vertex.label.degree=-pi/2.5, ...)
    invisible(g)
}
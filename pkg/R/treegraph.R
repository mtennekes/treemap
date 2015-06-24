#' Create a tree graph
#'
#' This function draws a tree graph. By default, a radial layout is used.
#' 
#' @param dtf a data.frame or data.table. Required.
#' @param index the index variables of dtf (see \code{\link{treemap}})
#' @param directed logical that determines whether the graph is directed (\code{TRUE}) or undirected (\code{FALSE})
#' @param palette.HCL.options list of advanced options to obtain Tree Colors from  the HCL space (when \code{palette="HCL"}). This list contains: 
#' \describe{
#'        \item{\code{hue_start}:}{number between 0 and 360 that determines the starting hue value (default: 30)}
#'        \item{\code{hue_end}:}{number between \code{hue_start} and \code{hue_start + 360} that determines the ending hue value (default: 390)}
#'        \item{\code{hue_perm}:}{boolean that determines whether the colors are permuted such that adjacent levels get more distinguishable colors. If \code{FALSE}, then the colors are equally distributed from \code{hue_start} to \code{hue_end} (default: TRUE)}
#'        \item{\code{hue_rev}:}{boolean that determines whether the colors of even-numbered branched are reversed (to increase discrimination among branches)}
#'        \item{\code{hue_fraction}:}{number between 0 and 1 that determines the fraction of the hue circle that is used for recursive color picking: if 1 then the full hue circle is used, which means that the hue of the colors of lower-level nodes are spread maximally. If 0, then the hue of the colors of lower-level nodes are identical of the hue of their parents. (default: .5)}
#'        \item{\code{chroma}:}{chroma value of colors of the first-level nodes, that are determined by the first index variable (default: 60)}
#'        \item{\code{luminance}:}{luminance value of colors of the first-level nodes, i.e. determined by the first index variable (default: 70)}
#'        \item{\code{chroma_slope}:}{slope value for chroma of the non-first-level nodes. The chroma values for the second-level nodes are \code{chroma+chroma_slope}, for the third-level nodes \code{chroma+2*chroma_slope}, etc. (default: 5)}
#'        \item{\code{luminance_slope}:}{slope value for luminance of the non-first-level nodes (default: -10)}} For "depth" and "categorical" types, only the first two items are used. Use \code{\link{treecolors}} to experiment with these parameters.
#' @param show.labels show the labels
#' @param rootlabel name of the rootlabel
#' @param vertex.layout layout algorithm name. See \code{\link[igraph:layout]{layout}} for options. The name corresponds to the layout function name after the period symbol, e.g. "auto", "random", etc. The default is "reingold.tilford" with a cirular layout.
#' @param vertex.layout.params list of arguments passed to \code{vertex.layout}
#' @param truncate.labels number of characters at which the levels are truncated. Either a single value for all index variables, or a vector of values for each index variable
#' @param vertex.size vertex.size (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param vertex.label.dist vertex.label.dist (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param vertex.label.cex vertex.label.cex (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param vertex.label.family vertex.label.family (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param vertex.label.color vertex.label.color (see \code{\link[igraph:igraph.plotting]{igraph.plotting}})
#' @param mai margins see \code{\link[graphics:par]{par}}
#' @param ... arguments passed to \code{\link[igraph:plot.igraph]{plot.igraph}}
#' @return (invisible) igraph object
#' @examples
#' data(business)
#' treegraph(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), show.labels=FALSE)
#' treegraph(business[business$NACE1=="F - Construction",], 
#'     index=c("NACE2", "NACE3", "NACE4"), show.labels=TRUE, truncate.labels=c(2,4,6))
#' treegraph(business[business$NACE1=="F - Construction",], 
#'     index=c("NACE2", "NACE3", "NACE4"), show.labels=TRUE, truncate.labels=c(2,4,6),
#'     vertex.layout="fruchterman.reingold")
#' @import data.table
#' @import igraph
#' @import colorspace
#' @export
treegraph <- function(dtf, index=names(dtf), directed=FALSE, palette.HCL.options, show.labels=FALSE, rootlabel="", vertex.layout="reingold.tilford", vertex.layout.params, truncate.labels=NULL, vertex.size=3, vertex.label.dist=0.3, vertex.label.cex=0.8, vertex.label.family="sans", vertex.label.color="black", mai=c(0,0,0,0), ...) {
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
    
    
    
    g <- graph.data.frame(dat[,c("parent", "current")], vertices=vdat, directed=directed)
    
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
    
    
    par(mai=mai)

    vertex.layout.function <- get(paste("layout", vertex.layout, sep="."))
    if (missing(vertex.layout.params)){
        vertex.layout.params <- if (vertex.layout=="reingold.tilford") {
            list(circular=T, root=1) 
        } else {
            list()
        }
    }
    vertex.layout.params = c(list(graph=g), vertex.layout.params)
    vertex.layout = do.call(vertex.layout.function, vertex.layout.params)
    plot(g, vertex.size=vertex.size, vertex.frame.color=NA, vertex.label=vdat_names, layout=vertex.layout
         , vertex.label.dist=vertex.label.dist, vertex.label.cex=vertex.label.cex, vertex.label.family=vertex.label.family, vertex.label.color=vertex.label.color, vertex.label.degree=-pi/2.5, ...)
    invisible(g)
}
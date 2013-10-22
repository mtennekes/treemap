#' Draw a tree graph
#'
#' Apply certain function to a tree structure.
#' 
#' @param dtf a data.frame that should contain only index variables. Required.
#' @param palette.HCL.options palette.HCL.options
#' @param show.labels show.labels
#' @param stack.labels stack.labels
#' @param rootlabel rootlabel
#' @param vertex.size vertex.size 
#' @param vertex.label.dist vertex.label.dist
#' @param vertex.label.cex vertex.label.cex
#' @param vertex.label.family vertex.label.family
#' @param vertex.label.color vertex.label.color
#' @return invisible igraph object
#' @import data.table
#' @import igraph
#' @import colorspace
treegraph <- function(dtf, index=names(dtf), palette.HCL.options, show.labels=FALSE, stack.labels= TRUE, rootlabel="", vertex.size=3, vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.family="sans", vertex.label.color="black") {
    k <- length(index)
    dat <- treepalette(dtf[, index],
                palette.HCL.options=palette.HCL.options,
                return.parameters=TRUE)[, 1:(k+1), with=FALSE]
    
    #dat$color <- color
    dat <- unique(dat)
    dat <- cbind(dat, as.data.table(treeid(dat[,1:k, with=FALSE])))
    vdat <- dat[,c("current", "color"), with=FALSE]
    vdat <- unique(vdat)
    rootname <- dat$parent[which(substr(dat$parent, 1, 2)=="NA")][1]
    vdat <- rbind(list(current=rootname, color=hcl(h=0, c=0, l=palette.HCL.options$luminance-palette.HCL.options$luminance_slope)), vdat)

    
    
    g <- graph.data.frame(dat[,c("current", "parent"), with=FALSE], vertices=vdat, directed=FALSE)
    #color <- color[match(get.vertex.attribute(g, "name"), dat$current)]
    #V(g)$color <- color[match(get.vertex.attribute(g, "name"), dat$current)]
    
    if (show.labels) {
            
        vdat_names <- gsub(".NA", "", vdat$current, fixed=TRUE)
        vdat_names[1] <- rootlabel

        if (!stack.labels) {
            ssres <- strsplit(vdat_names, split=".", fixed=TRUE)
            ssres[[1]] <- rootlabel
            vdat_names <- sapply(ssres, function(x)x[length(x)])
        }
        
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
    plot(g, vertex.size=vertex.size, vertex.frame.color=NA, vertex.label=vdat_names, layout=
             layout.reingold.tilford(g, circular=T, root=1), vertex.label.dist=vertex.label.dist, vertex.label.cex=vertex.label.cex, vertex.label.family=vertex.label.family, vertex.label.color=vertex.label.color, vertex.label.degree=-pi/2.5)
    invisible(g)
}
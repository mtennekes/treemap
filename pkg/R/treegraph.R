#' Draw a tree graph
#'
#' Apply certain function to a tree structure.
#' 
#' @param dat a data.frame or data.table that should contain only index variables. Required.
#' @param values vector with values that apply to the root node
#' @param depth variable of dat that indicated the node depths
#' @param fun function to be applied
#' @param prepare.dat data is by default preprocessed, except for interal use
#' @return list
#' @import data.table
#' @import igraph
#' @import colorspace
treegraph <- function(dat, palette.HCL.options, show.labels=FALSE, rootlabel="", vertex.size=3, vertex.label.dist=0.5, vertex.label.cex=0.8, vertex.label.family="sans", vertex.label.color="black") {
    k <- ncol(dat)
    
    dat <- treepalette(dat,
                palette.HCL.options=palette.HCL.options,
                return.parameters=TRUE)[, 1:(k+1), with=FALSE]
    
    #dat$color <- color
    dat <- unique(dat)
    dat <- cbind(dat, as.data.table(treeid(dat[,1:k, with=FALSE])), stringsAsFactors=FALSE)
    vdat <- dat[,c("current", "color"), with=FALSE]
    vdat <- unique(vdat)
    rootname <- dat$parent[which(substr(dat$parent, 1, 2)=="NA")][1]
    vdat <- rbind(list(current=rootname, color="#FFFFFF"), vdat)
    
    g <- graph.data.frame(dat[,c("current", "parent"), with=FALSE], vertices=vdat, directed=FALSE)
    #color <- color[match(get.vertex.attribute(g, "name"), dat$current)]
    #V(g)$color <- color[match(get.vertex.attribute(g, "name"), dat$current)]
    
    if (show.labels) {
        vdat_names <- gsub(".NA", "", vdat$current, fixed=TRUE)
        vdat_names[1] <- rootlabel
        
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
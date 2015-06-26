#' Obtain hierarchical color palettes (Tree Colors)
#'
#' Obtain hierarchical color palettes, either the so-called Tree Colors from the HCL color space model, or by using an existing color palette. The former method, which is recommended, is used by default in \code{\link{treemap}} (type \code{"index"}) and \code{\link{treegraph}}. Use \code{\link{treecolors}} to experiment with this method.
#' 
#' @param dtf a data.frame or data.table. Required.
#' @param index the index variables of dtf
#' @param method used method: either \code{"HCL"} (recommended), which is based on the HCL color space model, or \code{"HSV"}, which uses the argument \code{palette}.
#' @param palette color palette, which is only used for the HSV method
#' @param palette.HCL.options list of options to obtain Tree Colors from  the HCL space (when \code{palette="HCL"}). This list contains: 
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
#' @param return.parameters should a data.frame with color values and parameter options be returned (\code{TRUE}), or just the vector of color values (\code{FALSE})?
#' @param prepare.dat data is by default preprocessed, except for interal use
#' @return Either a vector of colors, or a data.frame is return (see \code{return.parameters}).
#' @import data.table
#' @import grid
#' @import colorspace
#' @export
treepalette <- function(dtf, index=names(dtf), method="HCL", palette=NULL, palette.HCL.options, return.parameters=TRUE, prepare.dat=TRUE) {
    .SD <- NULL #for CMD check
    
    palette.HCL.options <- tmSetHCLoptions(palette.HCL.options)
    k <- length(index)
    dat <- as.data.table(dtf)
    othercols <- setdiff(names(dat), index)
    if (length(othercols)) dat[, eval(othercols):=NULL]
    setcolorder(dat, index)
    
    dat[, names(dat):=lapply(.SD,as.factor)]
    if (prepare.dat) {
        if (k>1) {
            dats <- list()
            for (i in 1:(k-1)) {
                dats[[i]] <- dat[!duplicated(dat[,1:i, with=FALSE]), ]
                for (j in (i+1):k) dats[[i]][[j]] <- factor(NA, levels=levels(dats[[i]][[j]]))
            }
            dat <- rbindlist(c(list(dat), dats))
        }        
        dat <- dat[!duplicated(dat), ]

        # sort dat to be consistent with tmAggregate
        dep <- treedepth(dat)
        unikey <- do.call("paste", c(as.list(dat), list(dep, sep="__")))
        dat <- dat[order(unikey), ]
    }
    
    if (method=="HCL") {
        res <- treeapply(dat, list(lb=as.integer(palette.HCL.options$hue_start), 
                                   ub=as.integer(palette.HCL.options$hue_end),
                                   rev=FALSE), 
                         fun="addRange", frc=palette.HCL.options$hue_fraction, 
                         hue_perm=palette.HCL.options$hue_perm, hue_rev=palette.HCL.options$hue_rev)
        
        point <- with(res, (lb+ub)/2)
        chr <- palette.HCL.options$chroma + 
            palette.HCL.options$chroma_slope * (res$l-1)
        #75 - (k-res$l) * 10
        lum <- palette.HCL.options$luminance + 
            palette.HCL.options$luminance_slope * (res$l-1)
        #lum <- 95 - res$l * 10 #90
        color <- hcl(point,c=chr, l=lum)
        if (return.parameters) {
            return(cbind(as.data.frame(dat), data.table(HCL.color=color, 
                                         HCL.H=point,
                                         HCL.C=chr,
                                         HCL.L=lum,
                                         HCL.hue_lb=res$lb, 
                                         HCL.hue_ub=res$ub)))
        } else {
            return(color)
        }
    } else if (method=="HSV") {
        nl <- nlevels(dat[[1]])
        
        palette <- substr(palette, 1, 7) # remove alpha number
        palette <- rep(palette, length.out=nl)
        co <- coords(as(hex2RGB(palette), "HSV"))
        value <- as.list(as.data.frame(co))
        
        res <- treeapply(dat, value, fun="hsvs")
        color <- with(res, hex(HSV(H, S, V)))
        if (return.parameters) {
            return(cbind(as.data.frame(dat), data.frame(HSV.color=color, 
                                         HSV.H=res$H, 
                                         HSV.S=res$S,
                                         HSV.V=res$V)))
        } else {
            return(color)
        }    
    }
}
#' Hierarchical color palettes
#'
#' Create hierarchical color palettes, either by using the HCL color space model, or by using an existing color palette with the HSV space.
#' 
#' @param dat a data.frame that should contain only index variables. Required.
#' @param method used method: either \code{"HCL"} (recommended), which is based on the HCL color space model, or \code{"HSV"}, which uses the argument \code{palette}.
#' @param palette color palette, which is only used for the HSV method
#' @param palette.HCL.options list of advanced options to pick colors from  the HCL space (when \code{method="HCL"}). This list contains: 
#' \describe{
#'        \item{\code{hue_start}:}{number between 0 and 360 that determines the starting hue value (default: 30)}
#'        \item{\code{hue_end}:}{number between \code{hue_start} and \code{hue_start + 360} that determines the ending hue value (default: 390)}
#'        \item{\code{hue_spread}:}{boolean that determines whether the colors are spread such that adjacent levels get more distinguishable colors. If \code{FALSE}, then the colors are equally distributed from \code{hue_start} to \code{hue_end} (default: TRUE)}
#'        \item{\code{hue_fraction}:}{number between 0 and 1 that determines the fraction of the hue circle that is used for recursive color picking: if 1 then the full hue circle is used, which means that the hue of the colors of lower-level nodes are spread maximally. If 0, then the hue of the colors of lower-level nodes are identical of the hue of their parents. (default: .5)}
#'        \item{\code{chroma}:}{chroma value of colors of the first-level nodes, that are determined by the first index variable (default: 60)}
#'        \item{\code{luminance}:}{luminance value of colors of the first-level nodes, i.e. determined by the first index variable (default: 70)}
#'        \item{\code{chroma_slope}:}{slope value for chroma of the non-first-level nodes. The chroma values for the second-level nodes are \code{chroma+chroma_slope}, for the third-level nodes \code{chroma+2*chroma_slope}, etc. (default: 5)}
#'        \item{\code{luminance_slope}:}{slope value for luminance of the non-first-level nodes (default: -10)}} For "depth" and "categorical" types, only the first two items are used.
#' @param return.parameters should a data.frame with color values and parameter options be returned (\code{TRUE}), or just the vector of color values (\code{FALSE})?
#' @param prepare.dat data is by default preprocessed, except for interal use
#' @return Either a vector of colors, or a data.frame is return (see \code{return.parameters}).
#' @import data.table
#' @import grid
#' @import colorspace
#' @export
treepalette <- function(dat, method="HCL", palette=NULL, palette.HCL.options, return.parameters=FALSE, prepare.dat=TRUE) {
    require(data.table)
    k <- ncol(dat)
    if (method=="HCL") {
        if (prepare.dat) if (is.data.table(dat)) {
            dat[, names(dat):=lapply(.SD,as.factor)]
        } else {
            dat <- lapply(dat, as.factor)
            dat <- as.data.table(dat)
            dats <- list()
            for (i in 1:(k-1)) {
                dats[[i]] <- dat[!duplicated(dat[,1:i, with=FALSE]), ]
                for (j in (i+1):k) dats[[i]][[j]] <- factor(NA, levels=levels(dats[[i]][[j]]))
            }
            dat <- rbindlist(c(list(dat), dats))
            dat <- dat[!duplicated(dat), ]
            setkeyv(dat, names(dat))
        }
        
        res <- treeapply(dat, list(lb=palette.HCL.options$hue_start, 
                                   ub=palette.HCL.options$hue_end,
                                   rev=FALSE), 
                         fun="addRange", frc=palette.HCL.options$hue_fraction,
                         prepare.dat=prepare.dat)
        
        point <- with(res, (lb+ub)/2)
        chr <- palette.HCL.options$chroma + 
            palette.HCL.options$chroma_slope * (res$l-1)
        #75 - (k-res$l) * 10
        lum <- palette.HCL.options$luminance + 
            palette.HCL.options$luminance_slope * (res$l-1)
        #lum <- 95 - res$l * 10 #90
        color <- hcl(point,c=chr, l=lum)
        if (return.parameters) {
            return(cbind(dat, data.table(color=color, 
                                         H=point,
                                         C=chr,
                                         L=lum,
                                         hue_lb=res$lb, 
                                         hue_ub=res$ub, 
                                         stringsAsFactors=FALSE)))
        } else {
            return(color)
        }
    } else if (method=="HSV") {
        nl <- nlevels(dat[[1]])
        
        palette <- substr(palette, 1, 7) # remove alpha number
        palette <- rep(palette, length.out=nl)
        
        co <- coords(as(hex2RGB(palette), "HSV"))
        value <- as.list(as.data.frame(co))
        
        res <- treeapply(dat, value, fun="hsvs", prepare.dat=prepare.dat)
        color <- with(res, hex(HSV(H, S, V)))
        if (return.parameters) {
            return(cbind(dat, data.frame(color=color, 
                                         H=res$H, 
                                         S=res$S,
                                         V=res$V,
                                         stringsAsFactors=FALSE)))
        } else {
            return(color)
        }    
    }
}
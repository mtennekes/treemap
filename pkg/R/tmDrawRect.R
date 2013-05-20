tmDrawRect <- function(datlist, vps, indexList, lowerbound.cex.labels, inflate.labels, bg.labels, force.print.labels, cex_indices) {
    pushViewport(vps$vpDat, vps$vpDatAsp)
    
    
    drawRecs <- function(recs) {
        if (!is.na(recs$recs)[1]) grid.draw(recs$recs)
        if (!is.na(recs$txtbg)[1]) grid.draw(recs$txtbg)
        if (!is.na(recs$txt)[1]) grid.draw(recs$txt)
    }
    
    depth <- length(indexList)
    
    if (depth==1) {
        whichFill <- rep(TRUE, nrow(datlist))
        recs_fill <- createRec(datlist, 
                               filled=TRUE, 
                               label="normal", 
                               labellb=lowerbound.cex.labels, 
                               lwd = 1,
                               inflate.labels=inflate.labels,
                               force.print.labels=force.print.labels,
                               cex_index=cex_indices[1])
        grid.draw(recs_fill$recs)
        grid.draw(recs_fill$txt)
    } else {
        whichBold <- datlist$l==1
        lwds <- depth - datlist$l + 1
        whichFill <- datlist$l==depth
        
        whichNA <- is.na(datlist$n)
        
        recs_fill_NA <- createRec(datlist[whichFill & !whichBold & whichNA,], 
                                  filled=TRUE, 
                                  label="", 
                                  lwd = lwds[whichFill & !whichBold & whichNA], 
                                  inflate.labels=inflate.labels,
                                  force.print.labels=force.print.labels, 
                                  cex_index=cex_indices[3])
        
        recs_fill_norm <- createRec(datlist[whichFill & !whichBold &!whichNA,], 
                                    filled=TRUE, 
                                    label="normal", 
                                    labellb=lowerbound.cex.labels, 
                                    lwd = lwds[whichFill & !whichBold &!whichNA], 
                                    inflate.labels=inflate.labels,
                                    force.print.labels=force.print.labels, 
                                    cex_index=cex_indices[3])
        
        #browser()
        
        rng <- rev(sort(unique(datlist$l[!whichFill & !whichBold & !whichNA])))
        
        recs_trans_norm <- lapply(rng, function(r) createRec(datlist[!whichFill & !whichBold & !whichNA & datlist$l==r,], 
                                     filled=FALSE, 
                                     label="normal",
                                     labellb=lowerbound.cex.labels, 
                                     bg.labels = bg.labels, 
                                     lwd = lwds[!whichFill & !whichBold & !whichNA],
                                     inflate.labels=inflate.labels,
                                     force.print.labels=force.print.labels, 
                                     cex_index=cex_indices[2])) 
        
        recs_trans_bold <- createRec(datlist[!whichFill & whichBold,], 
                                     filled=FALSE, 
                                     label="bold", 
                                     labellb=lowerbound.cex.labels, 
                                     bg.labels = bg.labels, 
                                     lwd = lwds[!whichFill & whichBold], 
                                     inflate.labels=inflate.labels,
                                     force.print.labels=force.print.labels, 
                                     cex_index=cex_indices[1]) 
        
        if (length(rng)) {
            covers <- lapply(1:length(rng), function(i) 
                overlap(recs_fill_norm$txtbg, recs_trans_norm[[i]]$txtbg))
            covers[[length(rng)+1]] <- 
                overlap(recs_fill_norm$txtbg, recs_trans_bold$txtbg)
            cover <- do.call("mapply", 
                             args=c(covers, list(FUN=any, SIMPLIFY=TRUE)))
            if (!is.na(cover[1])) {
                recs_fill_norm$txt$gp$col[cover] <- NA
                recs_fill_norm$bg$gp$fill[cover] <- NA
            }
        }
        
        drawRecs(recs_fill_NA)
        drawRecs(recs_fill_norm)
        lapply(recs_trans_norm, drawRecs)
        drawRecs(recs_trans_bold)
        
    }
}
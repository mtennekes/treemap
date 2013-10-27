tmDrawRect <- function(datlist, vps, indexList, lowerbound.cex.labels, inflate.labels, bg.labels, force.print.labels, cex_indices, overlap.labels, lwds, border.col, font.labels) {
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
                               lwd = lwds[3],
                               inflate.labels=inflate.labels,
                               force.print.labels=force.print.labels,
                               cex_index=cex_indices[1],
                               border.col=border.col,
                               font.labels=font.labels)
        grid.draw(recs_fill$recs)
        grid.draw(recs_fill$txt)
    } else {
        whichBold <- datlist$l==1
        whichFill <- datlist$l==depth
        lwds2 <- lwds[ifelse(whichBold, 1, ifelse(whichFill, 3, 2))]

        whichNA <- is.na(datlist$n)
        #browser()
        recs_fill_NA <- createRec(datlist[whichFill & !whichBold & whichNA,], 
                                  filled=TRUE, 
                                  label="", 
                                  lwd = lwds2[whichFill & !whichBold & whichNA], 
                                  inflate.labels=inflate.labels,
                                  force.print.labels=force.print.labels, 
                                  cex_index=cex_indices[3], 
                                  border.col=border.col,
                                  font.labels=font.labels)
        
        recs_fill_norm <- createRec(datlist[whichFill & !whichBold &!whichNA,], 
                                    filled=TRUE, 
                                    label="normal", 
                                    labellb=lowerbound.cex.labels, 
                                    lwd = lwds2[whichFill & !whichBold &!whichNA], 
                                    inflate.labels=inflate.labels,
                                    force.print.labels=force.print.labels, 
                                    cex_index=cex_indices[3],
                                    border.col=border.col,
                                    font.labels=font.labels)
        
        #browser()
        
        rng <- rev(sort(unique(datlist$l[!whichFill & !whichBold & !whichNA])))
        
        recs_trans_norm <- lapply(rng, function(r) createRec(datlist[!whichFill & !whichBold & !whichNA & datlist$l==r,], 
                                     filled=FALSE, 
                                     label="normal",
                                     labellb=lowerbound.cex.labels, 
                                     bg.labels = bg.labels, 
                                     lwd = lwds2[!whichFill & !whichBold &
                                                    !whichNA & datlist$l==r],
                                     inflate.labels=inflate.labels,
                                     force.print.labels=force.print.labels, 
                                     cex_index=cex_indices[2],
                                     border.col=border.col,
                                     font.labels=font.labels)) 
        
        recs_trans_bold <- createRec(datlist[!whichFill & whichBold,], 
                                     filled=FALSE, 
                                     label="bold", 
                                     labellb=lowerbound.cex.labels, 
                                     bg.labels = bg.labels, 
                                     lwd = lwds2[!whichFill & whichBold], 
                                     inflate.labels=inflate.labels,
                                     force.print.labels=force.print.labels, 
                                     cex_index=cex_indices[1],
                                     border.col=border.col,
                                     font.labels=font.labels) 
        if (overlap.labels < 1) {
            
            anyTransBold <- any(!whichFill & whichBold)
            anyTransNorm <- any(!whichFill & !whichBold & !whichNA)

            layers <- list(recs_fill_norm$txtbg)
            if (length(rng)) {
                layers <- c(layers, lapply(recs_trans_norm, function(x)x$txtbg))
            }
            layers <- c(layers, list(recs_trans_bold$txtbg))
            
            select <- lapply(layers, function(l)rep(TRUE, length(l$x$arg1)))
            for (i in length(layers):2) {
                for (j in (i-1):1) {
                    cover <- overlap(layers[[j]], layers[[i]], 
                                     overlap.labels, select=select[[i]])
                    select[[j]] <- select[[j]] & !cover
                }
            }
            recs_fill_norm$txt$gp$col[!select[[1]]] <- NA
            recs_fill_norm$txtbg$gp$fill[!select[[1]]] <- NA
            recs_fill_norm$bg$gp$fill[!select[[1]]] <- NA
            
            if (length(rng)) for (i in 2:(length(layers)-1)) {
                recs_trans_norm[[i-1]]$txt$gp$col[!select[[i]]] <- NA
                recs_trans_norm[[i-1]]$txtbg$gp$fill[!select[[i]]] <- NA
                recs_trans_norm[[i-1]]$bg$gp$fill[!select[[i]]] <- NA
            }
        }
        
        drawRecs(recs_fill_NA)
        drawRecs(recs_fill_norm)
        lapply(recs_trans_norm, drawRecs)
        drawRecs(recs_trans_bold)
        
    }
    upViewport(2)
}
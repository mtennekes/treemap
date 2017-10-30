tmDrawRect <- function(datlist, vps, indexList, lowerbound.cex.labels, inflate.labels, bg.labels, 
                       force.print.labels, cex_indices, overlap.labels, border.col, border.lwds, 
                       fontcolor.labels, fontface.labels, fontfamily.labels, 
                       align.labels, xmod.labels, ymod.labels, eval.labels) {
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
                               label=cex_indices!=0, 
                               labellb=lowerbound.cex.labels, 
                               lwd = border.lwds,
                               inflate.labels=inflate.labels,
                               force.print.labels=force.print.labels,
                               cex_index=cex_indices,
                               border.col=border.col,
                               fontcolor.labels=fontcolor.labels,
                               fontface.labels=fontface.labels,
                               fontfamily.labels=fontfamily.labels,
                               align.labels=align.labels[[1]], 
                               xmod.labels=xmod.labels[[1]], 
                               ymod.labels=ymod.labels[[1]],
                               eval.labels=eval.labels)
        grid.draw(recs_fill$recs)
        if (cex_indices!=0) grid.draw(recs_fill$txt)
    } else {
        whichBold <- datlist$l==1
        whichFill <- datlist$l==depth
        #lwds2 <- lwds[ifelse(whichBold, 1, ifelse(whichFill, 3, 2))]

        whichNA <- is.na(datlist$n)
        recs_fill_NA <- createRec(datlist[whichFill & !whichBold & whichNA,], 
                                  filled=TRUE, 
                                  label=FALSE, 
                                  lwd = border.lwds[depth], 
                                  inflate.labels=inflate.labels,
                                  force.print.labels=force.print.labels, 
                                  cex_index=cex_indices[depth], 
                                  border.col=border.col[depth],
                                  fontcolor.labels=fontcolor.labels[depth],
                                  fontface.labels=fontface.labels[depth],
                                  fontfamily.labels=fontfamily.labels,
                                  align.labels=align.labels[[depth]], 
                                  xmod.labels=xmod.labels[[depth]], 
                                  ymod.labels=ymod.labels[[depth]],
                                  eval.labels=eval.labels)
        
        recs_fill_norm <- createRec(datlist[whichFill & !whichBold &!whichNA,], 
                                    filled=TRUE, 
                                    label=TRUE, 
                                    labellb=lowerbound.cex.labels, 
                                    lwd = border.lwds[depth], 
                                    inflate.labels=inflate.labels,
                                    force.print.labels=force.print.labels, 
                                    cex_index=cex_indices[depth],
                                    border.col=border.col[depth],
                                    fontcolor.labels=fontcolor.labels[depth],
                                    fontface.labels=fontface.labels[depth],
                                    fontfamily.labels=fontfamily.labels,
                                    align.labels=align.labels[[depth]], 
                                    xmod.labels=xmod.labels[[depth]], 
                                    ymod.labels=ymod.labels[[depth]],
                                    eval.labels=eval.labels)
        
        rng <- rev(sort(unique(datlist$l[!whichFill & !whichBold & !whichNA])))
        
        recs_trans_norm <- lapply(rng, function(r) createRec(datlist[!whichFill & !whichBold & !whichNA & datlist$l==r,], 
                                     filled=FALSE, 
                                     label=TRUE,
                                     labellb=lowerbound.cex.labels, 
                                     bg.labels = bg.labels, 
                                     lwd = border.lwds[r],
                                     inflate.labels=inflate.labels,
                                     force.print.labels=force.print.labels, 
                                     cex_index=cex_indices[r],
                                     border.col=border.col[r],
                                     fontcolor.labels=fontcolor.labels[r],
                                     fontface.labels=fontface.labels[r],
                                     fontfamily.labels=fontfamily.labels,
                                     align.labels=align.labels[[r]], 
                                     xmod.labels=xmod.labels[[r]], 
                                     ymod.labels=ymod.labels[[r]],
                                     eval.labels=eval.labels)) 
        
        recs_trans_bold <- createRec(datlist[!whichFill & whichBold,], 
                                     filled=FALSE, 
                                     label=TRUE, 
                                     labellb=lowerbound.cex.labels, 
                                     bg.labels = bg.labels, 
                                     lwd = border.lwds[1], 
                                     inflate.labels=inflate.labels,
                                     force.print.labels=force.print.labels, 
                                     cex_index=cex_indices[1],
                                     border.col=border.col[1],
                                     fontcolor.labels=fontcolor.labels[1],
                                     fontface.labels=fontface.labels[1],
                                     fontfamily.labels=fontfamily.labels,
                                     align.labels=align.labels[[1]], 
                                     xmod.labels=xmod.labels[[1]], 
                                     ymod.labels=ymod.labels[[1]],
                                     eval.labels=eval.labels) 
        if (overlap.labels < 1) {
            
            anyTransBold <- any(!whichFill & whichBold)
            anyTransNorm <- any(!whichFill & !whichBold & !whichNA)

            layers <- list(recs_fill_norm$txtbg)
            if (length(rng)) {
                layers <- c(layers, lapply(recs_trans_norm, function(x)x$txtbg))
            }
            layers <- c(layers, list(recs_trans_bold$txtbg))
            
            select <- lapply(layers, function(l)rep(TRUE, length(l$x)))
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
# Creates graphical rectangle objects out of coordinates
createRec <-
function(datlist, filled, label, bg.labels=220, labellb, lwd, inflate.labels, force.print.labels, 
         cex_index, border.col, fontcolor.labels, fontface.labels, fontfamily.labels, align.labels, xmod.labels, ymod.labels, eval.labels) {
#browser()
	if (nrow(datlist)==0) {
		return(list(recs=NA, txt=NA, txtbg=NA))
	}
	
	rgbcol <- col2rgb(as.character(datlist$color))
	if (filled) {
	    fill <- datlist$color
	    txtfill <- NA
	    txtRgb <- rgbcol
	} else if (is.numeric(bg.labels)) {
	    transp <- c(rep(bg.labels, nrow(datlist)))
	    rgbcol2 <- rgb(rgbcol["red",],rgbcol["green",],rgbcol["blue",],alpha=transp,maxColorValue=255)
	    txtfill <- as.character(rgbcol2)
	    txtRgb <- rgbcol
	    fill <- NA
	} else {
	    txtfill <- rep(bg.labels, nrow(datlist))
	    txtRgb <- col2rgb(txtfill)
	    fill <- NA
	}
	
    
    
	recs <- rectGrob(x=unit(datlist$x0,"npc"), y=unit(datlist$y0,"npc"), width=unit(datlist$w,"npc"), 
		height=unit(datlist$h,"npc"), just=c("left","bottom"), name=datlist$n, gp = gpar(lwd=lwd, lex=1, fill = fill, col=border.col))
	
	if (label) {
        light <- apply(txtRgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
		tCol <- if (is.null(fontcolor.labels)) ifelse(light, "black", "white") else rep(fontcolor.labels, length(light))

		noText <- recs$name == ""
		recs$name[noText] <- " "
		txt <- str2rect(recs, fontcol=tCol, fill=txtfill, fontface=fontface.labels, fontfamily=fontfamily.labels, inflate.labels=inflate.labels, 
                        cex_index=cex_index, align.labels=align.labels, xmod.labels=xmod.labels, ymod.labels=ymod.labels, eval.labels=eval.labels)
		
        
        txt$txt$gp$col[noText] <- NA
		txt$bg$gp$fill[noText] <- NA
		
		tooSmall <- txt$txt$gp$cex < labellb
		if (force.print.labels) {
			txt$txt$gp$cex[tooSmall] <- labellb
			txt$bg$gp$fill[tooSmall] <- NA
		} else {
			txt$txt$gp$col[tooSmall] <- NA
			txt$bg$gp$fill[tooSmall] <- NA
		}

	} else {
		txt <- list()
		txt$txt <- NA
		txt$bg <- NA
	}

	
	return(list(recs=recs, txt=txt$txt, txtbg=txt$bg))
}


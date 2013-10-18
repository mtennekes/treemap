# Creates graphical rectangle objects out of coordinates
createRec <-
function(datlist, filled, label, bg.labels=220, labellb, lwd, inflate.labels, force.print.labels, cex_index) {
#browser()
	if (nrow(datlist)==0) {
		return(list(recs=NA, txt=NA, txtbg=NA))
	}
# 	transp <- c(rep(200,nrow(datlist)))
# 	rgbcol <- col2rgb(as.character(datlist$color))
# 	rgbcol2 <- rgb(rgbcol["red",],rgbcol["green",],rgbcol["blue",],alpha=transp,maxColorValue=255)
# 
# 	fill <- datlist$color
# 	
# 	if (is.numeric(bg.labels)) {
# 		txtfill <- as.character(rgbcol2)
# 		txtRgb <- rgbcol
# 	} else {
# 
# 		txtfill <- rep(bg.labels, nrow(datlist))
# 		txtRgb <- col2rgb(txtfill)
# 		
# 	}
# 	
# 	if (!filled) {
# 		fill <- NA
# 	}
	
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
		height=unit(datlist$h,"npc"), just=c("left","bottom"), name=datlist$n, gp = gpar(lwd=lwd, lex=1,fill = fill))
	
	if (label != "") {
        light <- apply(txtRgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
		tCol <- ifelse(light, "black", "white")

		noText <- recs$name == ""
		recs$name[noText] <- " "
		txt <- str2rect(recs, fontcol=tCol, fill=txtfill, bold=( label=="bold"), inflate.labels=inflate.labels, cex_index=cex_index)
		
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


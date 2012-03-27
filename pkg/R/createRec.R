# Creates graphical rectangle objects out of coordinates
createRec <-
function(recList, filled, label, labelbg=TRUE, labellb, lwd, inflate.labels, force.print.labels, cex_index) {
#browser()
	if (nrow(recList)==0) {
		return(list(recs=NA, txt=NA, txtbg=NA))
	}
	transp <- c(rep(255,nrow(recList)))
	rgbcol <- col2rgb(as.character(recList$color))
	rgbcol2 <- rgb(rgbcol["red",],rgbcol["green",],rgbcol["blue",],alpha=transp,maxColorValue=255)

	fill <- as.character(rgbcol2)
	
	if (labelbg) {
		txtfill <- fill
	} else {
		txtfill <- NA
	}
	
	if (!filled) {
		fill <- NA
	}
	
	recs <- rectGrob(x=unit(recList$x0,"npc"), y=unit(recList$y0,"npc"), width=unit(recList$w,"npc"), 
		height=unit(recList$h,"npc"), just=c("left","bottom"), name=recList$ind, gp = gpar(lwd=lwd, lex=1,fill = fill))
	
	if (label != "") {
		if (filled || labelbg) {
			light <- apply(rgbcol, MARGIN=2, mean) >= 128
			tCol <- ifelse(light, "black", "white")
		} else {
			tCol <- c(rep("black",length(recs$x)))
		}
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


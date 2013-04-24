# Creates graphical rectangle objects out of coordinates
createRec <-
function(datlist, filled, label, labelbg=NA, labellb, lwd, inflate.labels, force.print.labels, cex_index) {
#browser()
	if (nrow(datlist)==0) {
		return(list(recs=NA, txt=NA, txtbg=NA))
	}
	transp <- c(rep(255,nrow(datlist)))
	rgbcol <- col2rgb(as.character(datlist$color))
	rgbcol2 <- rgb(rgbcol["red",],rgbcol["green",],rgbcol["blue",],alpha=transp,maxColorValue=255)

	fill <- as.character(rgbcol2)
	
	if (is.na(labelbg)) {
		txtfill <- fill
		txtRgb <- rgbcol
	} else {
# 		labelbgRgb <- col2rgb(labelbg, alpha=TRUE)
# 		
# 		txtRgb <- apply(rgbcol, MARGIN=2, FUN=function(x, y, t){
# 			x*(255-t)/255 + y*(t)/255}, labelbgRgb[1:3], labelbgRgb[4])
# 		txtfill <- apply(txtRgb, MARGIN=2, FUN=function(x){rgb(x[1], x[2], x[3], maxColorValue=255)} )

		txtfill <- rep(labelbg, nrow(datlist))
		txtRgb <- col2rgb(txtfill)
		
	}
	
	if (!filled) {
		fill <- NA
	}
	
	recs <- rectGrob(x=unit(datlist$x0,"npc"), y=unit(datlist$y0,"npc"), width=unit(datlist$w,"npc"), 
		height=unit(datlist$h,"npc"), just=c("left","bottom"), name=datlist$n, gp = gpar(lwd=lwd, lex=1,fill = fill))
	
	if (label != "") {
		light <- apply(txtRgb, MARGIN=2, mean) >= 128
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


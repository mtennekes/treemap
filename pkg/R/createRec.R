#' Creates graphical rectangle objects out of coordinates
#'
#' Creates graphical rectangle objects out of coordinates
#' @param recList
#' @param filled
#' @param label
#' @param labelbg
#' @param labellb
#' @param lwd
createRec <-
function(recList, filled=TRUE, label="", labelbg=FALSE, labellb=0, lwd=1) {
#browser()
	if (nrow(recList)==0) {
		return(list(recs=NA, txt=NA, txtbg=NA))
	}
	transp <- c(rep(255,nrow(recList)))
	rgbcol <- col2rgb(as.character(recList$color))
	rgbcol2 <- rgb(rgbcol["red",],rgbcol["green",],rgbcol["blue",],alpha=transp,max=255)

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
			maxCol <- mapply(as.integer(rgbcol[1,]),as.integer(rgbcol[2,]),as.integer(rgbcol[3,]),FUN="max")
			minCol <- mapply(as.integer(rgbcol[1,]),as.integer(rgbcol[2,]),as.integer(rgbcol[3,]),FUN="min")
			lightness <- floor(.5*(maxCol+minCol))
			tCol <- c(rep("black",length(recs$x)))
			tCol[lightness<128] <- "white"
		} else {
			tCol <- c(rep("black",length(recs$x)))
		}
		txt <- str2rect(recs, fontcol=tCol, fill=txtfill, bold=( label=="bold"), enlargable=FALSE)
		txt$txt$gp$col[txt$txt$gp$cex < labellb] <- NA
		txt$bg$gp$fill[txt$txt$gp$cex < labellb] <- NA

	} else {
		txt <- list()
		txt$txt <- NA
		txt$bg <- NA
	}

	
	return(list(recs=recs, txt=txt$txt, txtbg=txt$bg))
}


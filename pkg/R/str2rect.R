# Places text in rectangles
#
# Places text in rectangles
#
# @param grb rectGrob object defining the rectangle(s), where \code{grb$name} is the text to display
# @param fontcol vector defining the colors of the text
# @param fill vector defining the backgroud colors of the text
# @param bold logical defining whe the text is bold
# @param inflate.labels logical defining whether the textsize may exceed \code{cex=1}
str2rect <-
function(grb, fontcol, fill, bold, inflate.labels, cex_index) {
	# wrap text to 1-5 sentences
	txtWraps <- lapply(grb$name, FUN=function(txt) {
		txtWrap <- list(txt)
		txtWrap[2:5] <- sapply(2:5, FUN=function(x, txt) {
			sq <- seq(1,5,by=2)
			results <- lapply(sq, FUN=function(pos, x, txt) {
			strwrap(txt, width = pos+(nchar(txt)/x))}, x, txt)
			lengths <- sapply(seq_along(sq), FUN=function(x)length(results[[x]]))
			results <- (results[lengths==x])[1]},txt)
		txtWrap <- sapply(txtWrap, FUN=paste, collapse="\n")
		strID <- which(txtWrap!="")
		nLines <- (1:5)[strID]
		txtWrap <- txtWrap[strID]
		return(list(txt=txtWrap, lines=nLines))})

	# select succesfully wrapped text

	# get size of viewport
	inchesW <- convertWidth(grb$width, "inches", valueOnly=TRUE)
	inchesH <- convertHeight(grb$height, "inches", valueOnly=TRUE)

	# calculate heights and widths of text
	gp <- get.gpar()
	results <- mapply(txtWraps, inchesW, inchesH, FUN=function(wrap, inchesW, inchesH) {
		txtH <- convertHeight(unit(1,"lines"), "inches", valueOnly=TRUE) * (wrap$lines-0.25) * gp$lineheight
		txtW <- convertWidth(stringWidth(wrap$txt), "inches", valueOnly=TRUE)

		incrW <- (inchesW / txtW)
		incrH <- (inchesH / txtH)
		incr <- pmin.int(incrH, incrW)
		
		# determine best fit
		if (inflate.labels) {
			aspR <- pmax.int(incrH, incrW) / incr
			winningStr <- which.min(aspR)
		} else {
			incr[incr>1] <- 1
			winningStr <- which.max(incr)[1]
		}
		return(list(txt=wrap$txt[winningStr], cex=incr[winningStr], lines=wrap$lines[winningStr]))
	})
	txt <- unlist(results[1,])
	cex <- unlist(results[2,]) * cex_index
	nlines <- unlist(results[3,])
	
	fontface <- ifelse(bold, "bold", "plain")
	txtGrb <- textGrob(txt, x=grb$x+0.5*grb$width, y=grb$y+0.5*grb$height, gp=gpar(cex=cex, fontface=fontface, col=fontcol))
	
	#just <- c("left", "bottom")
    #txtGrb <- textGrob(txt, x=grb$x, y=grb$y, just=just, gp=gpar(cex=cex, fontface=fontface, col=fontcol))
	
	
	
	txtGrbW <- mapply(txt, cex, FUN=function(x,y, fontface){
		convertWidth(grobWidth(textGrob(x, gp=gpar(cex=y, fontface=fontface))),"inches", valueOnly=TRUE)}, fontface, USE.NAMES=FALSE)
		
	
	tooLarge <- (txtGrbW > inchesW)
	txtGrb$gp$cex[tooLarge] <- txtGrb$gp$cex[tooLarge] * (inchesW[tooLarge]/txtGrbW[tooLarge]) * 0.9



	
	# background rect, height slightly extended

	bckH <- mapply(txt, txtGrb$gp$cex, nlines, FUN=function(x,y,z, fontface){
		convertHeight(grobHeight(textGrob(x, gp=gpar(cex=y, fontface=fontface))),"npc", valueOnly=TRUE) * z/(z-0.25)}, fontface, USE.NAMES=FALSE)
	bckW <- mapply(txt, txtGrb$gp$cex, FUN=function(x,y, fontface){
		convertWidth(grobWidth(textGrob(x, gp=gpar(cex=y, fontface=fontface))),"npc", valueOnly=TRUE)}, fontface, USE.NAMES=FALSE)

	bckGrb <- rectGrob(x=txtGrb$x, y=txtGrb$y, width=bckW, height=bckH, gp=gpar(fill=fill, col=NA))


	
	return(list(txt=txtGrb, bg=bckGrb))
}


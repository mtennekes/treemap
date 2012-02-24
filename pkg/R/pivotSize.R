pivotSize <-
function(dat, rec){
	names(rec)=c("X0","Y0", "W", "H")
	# determine whether layout is horizontal or vertical
	if (rec$W<rec$H) {
		flip <- TRUE
		tmp <- rec$H
		rec$H <- rec$W
		rec$W <- tmp
	} else {
		flip <- FALSE
	}

	# determine maximum value
	maxV <- max(dat$value)	
	maxI <- which(dat$value==maxV)[1]

	if (maxI==1) {
		list1 <- data.frame(index=integer(0),value=numeric(0))
	} else {
		list1 <- dat[1:(maxI-1),]  	
	}

	x0 <- rec$X0+rec$W*(sum(list1$value)/sum(dat$value))
	if (maxI<nrow(dat)){
		list23 <- dat[(maxI+1):nrow(dat),]
		bestRatio <- 1000
		bestwT <- 0
		besthT <- 0
		bestL2 <- data.frame(index=integer(0),value=numeric(0))
		bestL3 <- data.frame(index=integer(0),value=numeric(0))
		for (i in 1:(nrow(list23)+1)) {
			if (i==1) {
				list2T <- data.frame(index=integer(0),value=numeric(0))
			} else {
				list2T <- list23[1:(i-1),]  	
			}
			if (i>nrow(list23)) {
				list3T <- data.frame(index=integer(0),value=numeric(0))
			} else {
				list3T <- list23[i:nrow(list23),]
			}
			wT <- (rec$W-x0+rec$X0)*((sum(list2T$value)+maxV)/(sum(list23$value)+maxV)) 	
			hT <- rec$H*(maxV/(sum(list2T$value)+maxV))
			ratio <- max(wT,hT)/min(wT,hT)
			if (ratio < bestRatio) {
				bestRatio <- ratio
				bestwT <- wT
				besthT <- hT
				bestL2 <- list2T
				bestL3 <- list3T
			}
		}
		w <- bestwT
		h <- besthT
		list2 <- bestL2
		list3 <- bestL3
	} else {
		w <- rec$W-x0+rec$X0
		h <- rec$H
		list2 <- data.frame(index=integer(0),value=numeric(0))
		list3 <- data.frame(index=integer(0),value=numeric(0))
	}
	y0 <- rec$Y0+rec$H-h
	
	recList <- data.frame(ind=integer(0), x0=numeric(0), y0=numeric(0), w=numeric(0), h=numeric(0))
	
	if (flip) {
		recList <- rbind(recList,data.frame(ind=dat[maxI,]$index,x0=rec$X0,y0=rec$Y0+rec$W-x0+rec$X0-w,w=h,h=w))
	} else {
		recList <- rbind(recList,data.frame(ind=dat[maxI,]$index,x0=x0,y0=y0,w=w,h=h))
	}
	
	if (nrow(list1)>0) {
		if (flip) {
			recList1 <- pivotSize(list1, list(rec$X0,rec$Y0+rec$W-x0+rec$X0,rec$H,x0-rec$X0))
		} else {
			recList1 <- pivotSize(list1, list(rec$X0,rec$Y0,x0-rec$X0,rec$H))
		}
	} else {
		recList1 <- data.frame(ind=integer(0), x0=numeric(0), y0=numeric(0), w=numeric(0), h=numeric(0))
	}
	recList <- rbind(recList,recList1)

	if (nrow(list2)>0) {
		if (flip) {
			recList2 <- pivotSize(list2, list(rec$X0+h, rec$Y0+rec$W-x0+rec$X0-w,rec$H-h,w)) 	
		} else {
			recList2 <- pivotSize(list2, list(x0,rec$Y0,w,rec$H-h)) 	
		}
	} else {
		recList2 <- data.frame(ind=integer(0), x0=numeric(0), y0=numeric(0), w=numeric(0), h=numeric(0))
	}
	recList <- rbind(recList,recList2)

	if (nrow(list3)>0) {
		if (flip) {
			recList3 <- pivotSize(list3, list(rec$X0,rec$Y0,rec$H,rec$W-(x0-rec$X0)-w)) 	
		} else {
			recList3 <- pivotSize(list3, list(x0+w,rec$Y0,rec$W-(x0-rec$X0)-w,rec$H)) 	
		}
	} else {
		recList3 <- data.frame(ind=integer(0), x0=numeric(0), y0=numeric(0), w=numeric(0), h=numeric(0))
	}
	recList <- rbind(recList,recList3)
	
	return (recList)
}


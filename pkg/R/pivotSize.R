pivotSize <-
function(value, rec){
	# determine whether layout is horizontal or vertical
	if (rec[3]<rec[4]) {
		flip <- TRUE
		tmp <- rec[4]
		rec[4] <- rec[3]
		rec[3] <- tmp
	} else {
		flip <- FALSE
	}

	# determine maximum value
	maxV <- max(value)	
	maxI <- which(value==maxV)[1]

	if (maxI==1) {
		list1 <- numeric(0)
	} else {
		list1 <- value[1:(maxI-1)]  	
	}

	x0 <- rec[1]+rec[3]*(sum(list1)/sum(value))
	if (maxI<length(value)){
		list23 <- value[(maxI+1):length(value)]
		bestRatio <- 1000
		bestwT <- 0
		besthT <- 0
		bestL2 <- numeric(0)
		bestL3 <- numeric(0)
		for (i in 1:(length(list23)+1)) {
			if (i==1) {
				list2T <- numeric(0)
			} else {
				list2T <- list23[1:(i-1)]  	
			}
			if (i>length(list23)) {
				list3T <- numeric(0)
			} else {
				list3T <- list23[i:length(list23)]
			}
			wT <- (rec[3]-x0+rec[1])*((sum(list2T)+maxV)/(sum(list23)+maxV)) 	
			hT <- rec[4]*(maxV/(sum(list2T)+maxV))
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
		w <- rec[3]-x0+rec[1]
		h <- rec[4]
		list2 <- numeric(0)
		list3 <- numeric(0)
	}
	y0 <- rec[2]+rec[4]-h
	
	if (flip) {
		recList <- matrix(c(rec[1], rec[2]+rec[3]-x0+rec[1]-w, h, w), 
						   ncol=4, dimnames=list(names(value)[maxI], c("x0", "y0", "w", "h")))
	} else {
		recList <- matrix(c(x0, y0, w, h), 
						 	   ncol=4,
						  dimnames=list(names(value)[maxI], 
						  			  c("x0", "y0", "w", "h")))
	}
	
	if (length(list1)>0) {
		if (flip) {
			recList1 <- pivotSize(list1,
								  c(rec[1],
								  	 rec[2]+rec[3]-x0+rec[1],
								  	 W=rec[4],
								  	 x0-rec[1]))
		} else {
			recList1 <- pivotSize(list1,
								  c(rec[1],
								  	 rec[2],
								  	 x0-rec[1],
								  	 rec[4]))
		}
		recList <- rbind(recList,recList1)
	}

	if (length(list2)>0) {
		if (flip) {
			recList2 <- pivotSize(list2, 
								  c(rec[1]+h,
								  	 rec[2]+rec[3]-
								  	 	x0+rec[1]-w,
								  	 rec[4]-h,
								  	 w)) 	
		} else {
			recList2 <- pivotSize(list2, c(x0,rec[2],w,rec[4]-h)) 	
		}
		recList <- rbind(recList,recList2)
	}

	if (length(list3)>0) {
		if (flip) {
			recList3 <- pivotSize(list3,
								  c(rec[1],
								  	 rec[2],
								  	 rec[4],
								  	 rec[3]-(x0-rec[1])-w)) 	
		} else {
			recList3 <- pivotSize(list3,
								  c(x0+w,
								  	 rec[2],
								  	 rec[3]-(x0-rec[1])-w,
								  	 rec[4])) 	
		}
		recList <- rbind(recList,recList3)
	}
	
	return (recList)
}


tmGenerateRect <- function(datlist, vps, indexList, algorithm) {
    k <- NULL
    
    datWidth <- vps$datWidth
    datHeight <- vps$datHeight
    
    dataRec <- data.table(X0=0, Y0=0, W=datWidth, H=datHeight)
    
    
    datlist[, X0:=0]
    datlist[, Y0:=0]
    datlist[, W:=datWidth]
    datlist[, H:=datHeight]
    
    datlist[, x0:=0]
    datlist[, y0:=0]
    datlist[, w:=0]
    datlist[, h:=0]
    
    x0 <- NULL; rm(x0); #trick R CMD check
    y0 <- NULL; rm(y0); #trick R CMD check
    w <- NULL; rm(w); #trick R CMD check
    h <- NULL; rm(h); #trick R CMD check
    
    X0 <- NULL; rm(X0); #trick R CMD check
    Y0 <- NULL; rm(Y0); #trick R CMD check
    W <- NULL; rm(W); #trick R CMD check
    H <- NULL; rm(H); #trick R CMD check
    
    fullname <- NULL; rm(fullname); #trick R CMD check
    .SD <- NULL; rm(.SD); #trick R CMD check
    level <- NULL; rm(level); #trick R CMD check
    color <- NULL; rm(color); #trick R CMD check
    clevel <- NULL; rm(clevel); #trick R CMD check
    se <- NULL; rm(se)
    
    depth <- length(indexList)
    
    
    subTM <- function(x) {
        rec <- unlist((x[1, list(X0, Y0, W, H)]))
        x <- x[order(x$i),]
        value <- x$s
        names(value) <- x$k
        
        recDat <- do.call(algorithm, list(value, rec))
        recNames <- row.names(recDat)
        recDat <- as.data.table(recDat)
        #recDat <- as.data.frame(recDat)
        recDat$k <- factor(recNames, levels=levels(x$k))
        setkeyv(recDat, "k")    		
    }

    for (i in 1:depth) {
        if (i!=1) {
            active <- datlist$l==i
            parents_active <- datlist$l==(i-1)
            iminusone <- i-1  # resolves data.table 1.9.5 compatibility issue
            parent_names <- apply(datlist[, paste0("index", 1:iminusone), with=FALSE], 
                                  1, paste, collapse="_")
            parents1 <- parent_names[active]
            parents2 <- parent_names[parents_active]
            matchID <- match(parents1, parents2)
            
            datlist[active, c("X0", "Y0", "W", "H"):= as.list(datlist[parents_active,][matchID, c("x0", "y0", "w", "h"), with=FALSE])]
            
            indList <- indexList[1:(i-1)]
            res <- datlist[active, subTM(.SD), by=indList]
        } else {
            res <- subTM(datlist[datlist$l==1])
        }
        setkey(res, k)
        datlist[match(res$k, datlist$k), c("x0", "y0", "w", "h"):= as.list(res[, c("x0", "y0", "w", "h"), with=FALSE])]
        
    }
    
    # convert to npc (0 to 1)
    datlist[, x0:=x0/dataRec$W]
    datlist[, y0:=y0/dataRec$H]
    datlist[, w:=w/dataRec$W]
    datlist[, h:=h/dataRec$H]

    datlist[, c("X0", "Y0", "W", "H"):=NULL]
    
    datlist
}
tmAggregate <- function(dtfDT, indexList, type, ascending, na.rm) {

    depth <- length(indexList)
    
    dats <- list()
    for (d in 1:depth) {
        datd <- tmAggregateStep(dtfDT, indexList[1:d], na.rm=na.rm)
        if (d < depth) {
            indexPlus <- indexList[(d+1):depth]
            datd[, indexPlus] <- lapply(indexPlus, function(x)factor(NA))
            setcolorder(datd, c(indexList, "s", "c", "i"))
        }
        datd[, l:=d]
        datd <- datd[order(datd$i),]
        
        dats[[d]] <- datd
    }
    datlist <- do.call("rbind", dats)
    if (min(datlist$s) < 0) stop("Column vSize contains negative values.")
    
    datlist <- datlist[datlist$s>0,]
    if (type=="dens") {
        datlist[, c:=c/s]
        datlist[is.nan(datlist$c), c:=0]
    }
    if (!ascending) {
        datlist[, i:=-i]
    }
    
    # add unqiue key (k)
    datlist[, k:=as.factor(do.call("paste", c(as.list(datlist[, c(indexList, "l"), with=FALSE]), sep="__")))]
    setkey(datlist, k)
    
    #recList[, ind:=apply(recList, MARGIN=1, FUN=function(x) x[as.integer(x["l"])])]
    

    # add label name (n)
    datlist[, n:=apply(datlist, MARGIN=1, FUN=function(x) x[as.integer(x["l"])])]
    datlist[, n:=ifelse(is.na(n), "", n)]
    
    datlist    
}


tmAggregateStep <- function(dtfDT, indexList, na.rm) {
        
    
    
    ## aggregate numeric variable
    vars <- c("s", "c", "i")
    varsNum <- sapply(dtfDT, is.numeric)[vars]
    dat <- dtfDT[ , lapply(.SD[, vars[varsNum], with=FALSE], sum, na.rm=na.rm), 
                 by=indexList]
    
    ## aggregate categorical variables: for each aggregate, get the mode
    if (!all(varsNum)) {
        datCat <- dtfDT[ , lapply(.SD[, vars[!varsNum], with=FALSE],
                                  function(x) which.max(table(x))), by=indexList]
        dat <- data.table(dat, datCat[, vars[!varsNum], with=FALSE])
    }
    for (v in vars[!varsNum]) {
        dat[[v]] <- factor(dat[[v]], labels=levels(dtfDT[[v]]))
    }
    dat
}

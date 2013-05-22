tmAggregate <- function(dtfDT, indexList, type, ascending, na.rm) {

    depth <- length(indexList)
    dats <- list()
    for (d in 1:depth) {
        datd <- tmAggregateStep(dtfDT, indexList[1:d], na.rm=na.rm)
        if (d < depth) {
            indexPlus <- indexList[(d+1):depth]
            datd[, get("indexPlus"):=lapply(indexPlus, function(x)factor(NA, levels=levels(dtfDT[[x]])))]
            setcolorder(datd, c(indexList, "s", "c", "i"))
        }
        datd[, l:=d]
        
        dats[[d]] <- datd
    }
    datlist <- rbindlist(dats)
    if (any(is.na(datlist$s)) && !na.rm) stop("vSize contains missing values. Set na.rm=TRUE to neglect them")
    
    datlist <- datlist[!is.na(datlist$s), ]
    if (min(datlist$s) < 0) stop("vSize contains negative values.")
    
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
    
    # add label name (n)
    datlist[, n:=apply(datlist, MARGIN=1, FUN=function(x) x[as.integer(x["l"])])]
    datlist[, n:=ifelse(is.na(n), "", n)]
    
    datlist    
}


tmAggregateStep <- function(dtfDT, indexList, na.rm) {
        
    isCat <- !is.numeric(dtfDT$c)
    
    ## aggregate numeric variable
    fn <- function(x) {
        if (is.numeric(x)) {
            sum(x, na.rm=na.rm)
        } else {
            which.max(table(x))
        }
    }
    dat <- dtfDT[ , lapply(.SD[, list(s, c, i)], fn), by=indexList]
    
    ## aggregate categorical variables: for each aggregate, get the mode
    if (isCat) {
        #fact <- factor(datCat$c, levels=1:nlevels(dtfDT$c), labels=levels(dtfDT$c))
        dat[, c:=factor(c, levels=1:nlevels(dtfDT$c), labels=levels(dtfDT$c))]
    }
    dat
}

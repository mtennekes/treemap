aggTmData <- function(dtfDT, indexList, na.rm) {
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

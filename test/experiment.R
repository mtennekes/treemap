
data(GNI2010)

dat <- GNI2010
index <- as.list(dat[, c("country", "continent")])


treecolor <- function(index, fun) {
    n <- length(index[[1]])
    index <- lapply(index, as.factor)
    tapply(rep(1,n), INDEX=index, function(){
                
    })
    
}
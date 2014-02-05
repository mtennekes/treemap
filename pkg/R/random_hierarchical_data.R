random.hierarchical.data <- function(n=NULL, method="random", ave.number.children=4, children.root=4, depth=3, nodes.per.layer=NULL, children.per.layer=NULL, labels=c("LETTERS", "numbers", "letters"), sep=".") {
    
    # check parameters
    if (!missing(n)) {
        nchild <- n^(1/depth)
        n2 <- n - (n/nchild)
        nchild <- n2^(1/depth)
        
        nodes.per.layer <- nchild^(1:depth)
        nodes.per.layer <- round(nodes.per.layer * (n/sum(nodes.per.layer)))
        nodes.per.layer[depth] <- sum(nodes.per.layer[-depth])
        method <- "fixed.layers"
    }
    
    
    if (!(method %in% c("random", "fixed.tree", "fixed.layers")))
        stop("Incorrect method.")
    if (method=="random") {
        if (missing(ave.number.children))
            stop("Required argument ave.number.children not specified.")
    } else if (method=="fixed.tree") {
        if (missing(children.per.layer))
            stop("Required argument children.per.layer not specified.")
        if (length(children.per.layer)==1)
            children.per.layer <- rep(children.per.layer, depth) else
                depth <- length(children.per.layer)
    } else if (method=="fixed.layers") {
        if (missing(nodes.per.layer))
            stop("Required argument nodes.per.layer not specified.")
        if (length(nodes.per.layer)==1)
            nodes.per.layer <- rep(nodes.per.layer, depth) else
                depth <- length(nodes.per.layer)
    }
        

    # create list with first layer items
    l <- list(as.character(1:switch(method,
                                    random=children.root,
                                    fixed.tree=children.per.layer,
                                    fixed.layers=nodes.per.layer[1])))
    isleaf <- list()
    # fill tree        
    for (d in 2:depth) {
        parents <- l[[d-1]]
        nprev <- length(parents)

        x <- switch(method,
            random = rpois(nprev, lambda=ave.number.children),
            fixed.tree = rep(children.per.layer, nprev),
            fixed.layers = rep(1, nprev) + 
                table(factor(sample(3, children.per.layer[d]-nprev, 
                                    replace=TRUE), levels=1:nprev)))
        l <- c(l, list(unlist(mapply(function(y, z) if (z==0) NULL
                                     else paste(rep(y, z), 1:z, sep=sep),
                                     parents, x, SIMPLIFY=FALSE))))
        ## track leafs
        isleaf <- c(isleaf, list(x==0))
    }
    isleaf <- c(isleaf, rep(TRUE, length(l[[depth]])))
    
    # remove possible empty layers
    depth <- sum(sapply(l, length)!=0)
    l <- l[1:depth]
    isleaf <- isleaf[1:depth]
    labels <- rep(labels, length.out=depth)
    
    # keep leafs
    l <- mapply("[", l, isleaf)
    
    # create data.frame
    l <- lapply(l, function(ll) {
        if (!length(ll)) 
            return(NULL)
        spl <- lapply(ll, function(lll) strsplit(lll, split=sep, fixed=TRUE)[[1]])
        rows <- matrix(unlist(lapply(spl, function(spll) sapply(1:length(spll), function(x)paste(spll[1:x], collapse=sep)))), nrow=length(ll), byrow=TRUE)
        if (ncol(rows) < depth) rows <- cbind(rows, matrix(NA, nrow=length(ll), ncol=depth-ncol(rows)))
        
        as.data.frame(rows, stringsAsFactors=FALSE)
    })
    l <- l[do.call("order", l),]

    
    l <- add.labels(l, labels)
    
    l
}    
     
# function to add labels

add.labels <- function(l, labels) {
    # get maxima
    maxx <- sapply(l, function(ll) {
        max(as.integer(gsub(".*[.](.*)", "\\1", ll)), na.rm=TRUE)
    })
    
    lvls <- mapply(getSymbols, symbols, maxx)
    
    as.data.frame(lapply(l, function(ll) {
        spl <- lapply(ll, function(lll) strsplit(lll, split=sep, fixed=TRUE)[[1]])
        d <- max(sapply(spl, length))
        nas <- sapply(spl, function(x)is.na(x[1]))
        spl <- lapply(spl, function(x)rep(x, length.out=d))
        m <- matrix(as.integer(unlist(spl)), nrow=length(ll), byrow=TRUE)    
        
        m <- lapply(1:ncol(m), function(x)lvls[[x]][m[,x]])
        lvl <- do.call("paste", c(m, list(sep=sep)))
        lvl[nas] <- NA
        lvl
    }))
}



#' Function to return vector of sorted symbols
#' 
#' @param s one of "letters", "LETTERS", "numbers1", "numbers0", "numbers". The symbol set for numbers1 is 1:9, and for numbers0 it is 0:9. Numbers is equal to numbers0, except that is starts from 1.
#' @param n number of levels needed
get.symbols <- function(s, n) {
    sn <- switch(s, "letters"=26, "LETTERS"=29, "numbers1"=9, "numbers0"=10, "numbers"=10, "hex"=16, "bits"=2)
    symb <- switch(s, "letters"=letters, "LETTERS"=LETTERS, "numbers1"=1:9, "numbers0"=0:9, "numbers"=0:9, "hex"=c(0:9, LETTERS[1:6]), "bits"=0:1)
    
    if (s=="numbers") {
        ndigit <- which(n <= (sn^(1:4)-1))[1]
        lvls <- c(symb[-1], rep(symb, length.out=n))[1:n]
    } else {
        ndigit <- which(n <= sn^(1:4))[1]
        lvls <- rep(symb, length.out=n)
    }
    
    if (ndigit > 1) for (i in 2:ndigit) {
        lvls <- paste(rep(symb, each=sn^(i-1), length.out=n+1)[-1], lvls, sep="")
    }
    lvls
}
 

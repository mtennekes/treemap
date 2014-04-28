#' Create random hierarchical data
#' 
#' This function generates random hierarchical data. Experimental.
#' 
#' @param n number of leaf nodes. This is a shortcut argument. If specified, the method is set to \code{"random.arcs"} with a nodes.per.layer such that the average number of children per layer is as constant as possible.
#' @param method one of
#' \describe{
#'        \item{\code{"random"}:}{Random tree where for each node, the number of children, is determined by a random poisson generator with \code{lambda=number.children}, until the maximum depth specified by \code{depth} is reached. The number of children of the root node is set to \code{children.root}.}
#'        \item{\code{"random.arcs"}:}{Random tree where the exact number of nodes in each layer must be speficied by \code{nodes.per.layer}. The arcs between the layers are random, with the restriction that each node is connected.}
#'        \item{\code{"full.tree"}:}{Each node has exactly \code{number.children} children.}}
#' @param number.children the number of children. For \code{method="random"} this is the average number of children and for \code{method="full.tree"}, it is the exact number of children. In the latter case, it can also be a vector that specifies the number of children for each layer.
#' @param children.root number of children of the root node. For \code{method="random"} only.
#' @param depth depth of the tree. Note that for \code{method="random"}, this depth may not be reached.
#' @param nodes.per.layer exact number of nodes per layer, that is needed for \code{method="random.arcs"}
#' @param labels one of \code{"letters"}, \code{"LETTERS"}, \code{"numbers"}, \code{"numbers1"}, \code{"numbers0"}, \code{"hex"}, \code{"bits"}. The label set for \code{"numbers1"} is \code{1:9}, and for \code{"numbers0"} it is \code{0:9}. \code{"numbers"} is equal to \code{"numbers0"}, except that is starts from \code{1}.
#' @param labels.prefix vector of label prefixes, one for each layer
#' @param sep seperator character
#' @param colnames names of the columns. The first \code{depth} columns are the index columns (from highest to lowest hierarchical layer), and the last column is stored with random values
#' @param value.generator function that determine the random values for the leaf nodes
#' @param value.generator.args list of arguments passed to \code{value.generator}
#' @example ../examples/random_hierarchical_data.R
#' @export
random.hierarchical.data <- function(n=NULL, method="random", number.children=3, children.root=4, depth=3, nodes.per.layer=NULL, labels=c("LETTERS", "numbers", "letters"), labels.prefix=NULL, sep=".", colnames=c(paste("index", 1:depth, sep=""), "x"), value.generator=rlnorm, value.generator.args=NULL) {
    
    # check parameters
    if (!missing(n)) {
        nchild <- n^(1/depth)
        nodes.per.layer <- round(nchild^(1:depth))
        method <- "random.arcs"
    }
    
    if (!(method %in% c("random", "full.tree", "random.arcs")))
        stop("Incorrect method.")
    if (method=="random") {
        if (is.null(number.children))
            stop("Required argument ave.number.children not specified.")
    } else if (method=="full.tree") {
        if (is.null(number.children))
            stop("Required argument number.children not specified.")
        if (length(number.children)==1)
            number.children <- rep(number.children, depth) else
                depth <- length(number.children)
    } else if (method=="random.arcs") {
        if (is.null(nodes.per.layer))
            stop("Required argument nodes.per.layer not specified.")
        if (length(nodes.per.layer)==1)
            nodes.per.layer <- rep(nodes.per.layer, depth) else
                depth <- length(nodes.per.layer)
    }
        
    # create list with first layer items
    l <- list(as.character(1:switch(method,
                                    random=children.root,
                                    full.tree=number.children[1],
                                    random.arcs=nodes.per.layer[1])))
    isleaf <- list()
    # fill tree        
    for (d in 2:depth) {
        parents <- l[[d-1]]
        nprev <- length(parents)

        x <- switch(method,
            random = rpois(nprev, lambda=number.children),
            full.tree = rep(number.children[d], nprev),
            random.arcs = rep(1, nprev) + 
                table(factor(sample.int(nprev, nodes.per.layer[d]-nprev, 
                                    replace=TRUE), levels=1:nprev)))
        l <- c(l, list(unlist(mapply(function(y, z) if (z==0) NULL
                                     else paste(rep(y, z), 1:z, sep=sep),
                                     parents, x, SIMPLIFY=FALSE))))
        ## track leafs
        isleaf <- c(isleaf, list(x==0))
    }
    isleaf <- c(isleaf, list(rep(TRUE, length(l[[depth]]))))
    
    # remove possible empty layers
    depth <- sum(sapply(l, length)!=0)
    l <- l[1:depth]
    isleaf <- isleaf[1:depth]
    labels <- rep(labels, length.out=depth)
    if (!missing(labels.prefix)) labels.prefix <- rep(labels.prefix, length.out=depth)
    
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
    
    l <- do.call("rbind", l)
    l <- l[do.call("order", l),]

    
    l <- add.labels(l, labels, labels.prefix, sep)
    
    
    # add values
    l$x <- do.call("value.generator", args=c(list(nrow(l)), value.generator.args))
    
    # change colnames
    names(l) <- colnames[c(1:depth, ncol(l))]
    
    l
}    
     
# function to add labels
add.labels <- function(l, labels, labels.prefix, sep) {
    # get maxima
    maxx <- sapply(l, function(ll) {
        max(as.integer(gsub(".*[.](.*)", "\\1", ll)), na.rm=TRUE)
    })
    lvls <- mapply(get.symbols, labels, maxx, SIMPLIFY=FALSE)
    
    as.data.frame(lapply(l, function(ll) {
        spl <- lapply(ll, function(lll) strsplit(lll, split=sep, fixed=TRUE)[[1]])
        d <- max(sapply(spl, length))
        nas <- sapply(spl, function(x)is.na(x[1]))
        spl <- lapply(spl, function(x)rep(x, length.out=d))
        m <- matrix(as.integer(unlist(spl)), nrow=length(ll), byrow=TRUE)    
        
        m <- lapply(1:ncol(m), function(x)lvls[[x]][m[,x]])
        lvl <- do.call("paste", c(m, list(sep=sep)))
        lvl[nas] <- NA
        if (!is.null(labels.prefix)) if (labels.prefix[d]!="") lvl <- paste(labels.prefix[d], lvl)
        lvl
    }))
}



# Function to return vector of sorted symbols
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
 

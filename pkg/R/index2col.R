index2col <-
function(dat, position.legend, palette, labels, method="HSV") {
    
    
    
    
    dt <- copy(dat)
    
    depth <- sum(substr(names(dt), 1, 5)=="index")
    
    dt[, md:=apply(dt[, 1:depth, with=FALSE], MARGIN=1, FUN=function(x)sum(!is.na(x)))]

    
    indexList <- paste0("index", 1:depth)
    
    ## generic function for hierarchical structures
    ## dt = data.table
    ## index = list of index names
    ## values = named list of initial numeric values 
    ## each element can be a single element (i.e. that belongs to the root node), or a vector of the same size as the first index
    ## function to be applied
    treeapply <- function(dt, index, values, fun, ...) {
        args <- list(...)
        vars <- names(values)
        
        #setkeyv(dt, index)
        
        if (length(values[[1]])==1) {
            dt[, eval(vars):=values]
            # apply function on first layer
            dt[dt$l==1, eval(vars):=do.call(fun, args=c(list(as.list(dt[dt$l==1,vars,with=FALSE]), depth=1, maxdepth=dt$md[dt$l==1]), args))]
        } else {
            ind1 <- dt[[index[1]]]
            lvls <- levels(ind1)
            dt[, eval(vars):=1]
            for (l in 1:length(lvls)) {
                lv <- lvls[l]
                vls <- lapply(values, function(v)v[l])
                vls <- as.data.frame(vls)
                dt[ind1==lv, eval(vars):=vls]
            }
        }
        
        if (length(index)>1) for (d in 2:length(index)) {
            id <- index[1:(d-1)]
            # assign values of layer above to parent values of current layer
            #dt[dt$l==d, eval(parents):=children, by=id]
            fn <- function(x) {
                x <- copy(x)
                x[x$l==d, eval(vars):=as.list(x[x$l==(d-1), vars, with=FALSE])]
                x[x$l==d, eval(vars):=do.call(fun, args=c(list(as.list(x[x$l==d,vars,with=FALSE]), depth=d, maxdepth=x$md[x$l==d]), args))]
                x
            }
            res <- as.list(dt[, fn(.SD), by=id][, vars, with=FALSE])
            dt[, eval(vars):=res]
        }
        dt
    }
    
    ########## method 1: split hcl circle
    addRange <- function(x, depth, maxdepth, frc = 0.3) {
        #browser()
        LB <- x[[1]][1]
        UB <- x[[2]][2]
        
        nr <- length(x[[1]])
        
        sq <- seq(LB, UB, length.out=nr+1)
        step <- sq[2] - sq[1]
        spacer <- step * frc *.5
        
        list(lb=sq[1:nr]+spacer, ub=sq[2:(nr+1)]-spacer)
    }
    
    
    hsvs <- function(x, depth, maxdepth) {
        H <- x[[1]]
        S <- x[[2]]
        V <- x[[3]]
        
        nr <- length(H)
        hrng <- 10
        srng <- .05
        
        if (nr > 1) {
            # circle
#             r <- seq(0, 2*pi, length.out=nr+1)[1:nr]
#             H <- H + sin(r)*hrng
#             S <- S + cos(r)*srng
            # grid
            ncol <- ceiling(sqrt(nr))
            nrow <- ceiling(nr/ncol)
            H <- H + rep(seq(-hrng, hrng,length.out=nrow), times=ncol)[1:nr]
            S <- S + rep(seq(-srng, srng,length.out=ncol), each=nrow)[1:nr]
            
            
            H[H<0] <- H[H<0] + 360
            H[H>360] <- H[H>360] - 360
            S[S<0] <- 0
            S[S>1] <- 1
            V[maxdepth==depth] <- V[maxdepth==depth] * .75
        }
        list(H=H, S=S, V=V)
    }
    
    browser()
    
    if (method=="HCL") {
        dt <- treeapply(dt, indexList, list(lb=0, ub=1), "addRange")

        dt[, point:=(lb+ub)/2]
        dt[, color:=hcl(point*360)]
    } else if (method=="HSV") {
        require(colorspace)
        co <- coords(as(hex2RGB(palette), "HSV"))
        nl <- nlevels(dt[[1]])
        value <- lapply(as.list(as.data.frame(co)), function(x)x[1:nl])

        dt <- treeapply(dt, indexList, value, "hsvs")
        dt[, color:=hex(HSV(H, S, V))]
    }
    
    ## test
    #plot(dt$H, dt$S)
    
    
    
     
 	if (position.legend!="none") {
 	    labels <- dt$n[dt$l==1]
  	    colorl <- dt$color[dt$l==1]
 	    drawLegend(labels, colorl, position.legend=="bottom")
 	}
    
	
	return (dt$color)
}
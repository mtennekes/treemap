treedepth <- function(data) {
	k <- ncol(data)
	apply(data, MARGIN=1, FUN=function(x)k-sum(is.na(x)))
}

treeapply <- function(dat, values, depth=NULL, fun, prepare.dat=FALSE, ...) {
    .SD <- NULL
    
	if (prepare.dat) if (is.data.table(dat)) {
		dat[, names(dat):=lapply(.SD,as.factor)]
	} else {
		dat <- lapply(dat, as.factor)
		#require(data.table)
		dat <- as.data.table(dat)
	}
	dt <- dat[!duplicated(dat), ]
	
	k <- ncol(dt)

    # hierarchical depth
	if (missing(depth)) {
		dt[, l:=treedepth(dt)]
	} else {
		dt[, l:=depth]
	}
	
	args <- list(...)
	vars <- names(values)
	
	#setkeyv(dt, index)
	
	if (length(values[[1]])==1) {
		## apply function on first layer
		dt[, eval(vars):=values]
		dt[dt$l==1, eval(vars):=do.call(fun, 
										  args=c(list(as.list(dt[dt$l==1,vars,with=FALSE]), depth=1), args))]
	} else {
		## assign values for levels of  first layer
		ind1 <- dt[[1]]
		lvls <- levels(ind1)
		dt[, eval(vars):=1]
		for (l in 1:length(lvls)) {
			lv <- lvls[l]
			vls <- lapply(values, function(v)v[l])
			vls <- as.data.frame(vls)
			if (any(ind1==lv)) dt[ind1==lv, eval(vars):=vls]
		}
	}
	
	if (k>1) for (d in 2:k) {
		id <- names(dt)[1:(d-1)]
		# assign values of layer above to parent values of current layer
		#dt[dt$l==d, eval(parents):=children, by=id]
		fn <- function(x) {
		    x <- copy(x)
			x[x$l==d, eval(vars):=as.list(x[x$l==(d-1), vars, with=FALSE])]
			x[x$l==d, eval(vars):=do.call(fun, args=c(list(as.list(x[x$l==d,vars,with=FALSE]), depth=d), args))]
			x
		}
		res <- as.list(dt[, fn(.SD), by=id][, vars, with=FALSE])
		dt[, eval(vars):=res]
	}
	
	id1 <- treeid(dat)$current
	id2 <- treeid(dt[,1:k, with=FALSE])$current
	
	as.list(dt[match(id1,id2),(k+1):ncol(dt), with=FALSE])
}

########## method 1: split hcl circle
addRange <- function(x, depth, frc = .5) {
	LB <- x[[1]][1]
	UB <- x[[2]][1]
	
	nr <- length(x[[1]])
	#browser()
	sq <- seq(LB, UB, length.out=nr+1)
	spacer <- (sq[2] - sq[1]) * frc *.5

	s <- spread(nr)
	
	start <- sq[1:nr][s]
	end <- sq[2:(nr+1)][s]
	
	list(lb=start+spacer, ub=end-spacer)
}

########## method 2: modify fixed colors with hsv space
hsvs <- function(x, depth) {
    H <- x[[1]]
    S <- x[[2]]
    V <- x[[3]]
    nr <- length(H)
    hrng <- 10
    
    if (nr > 1) {
        ncol <- ceiling(sqrt(nr))
        nrow <- ceiling(nr/ncol)
        H <- H + seq(-hrng, hrng,length.out=nr)
        S <- S * 1.2
        
        H[H<0] <- H[H<0] + 360
        H[H>360] <- H[H>360] - 360
        S[S<0] <- 0
        S[S>1] <- 1
        V <- V * .9
        #V[maxdepth==depth] <- V[maxdepth==depth] * .75
    }
    list(H=H, S=S, V=V)
}


### spread cyclic vector 1:n, such that adjacent entries are kept at a distance. Based on the five-cycle: 1 3 5 2 4, where between any two original neighbors, there is an angle of 2/5*360=144 degrees.
spread <- function(n) {
    #for (n in 1:50) {
    if (n<5) {
        s <- 1:n
    } else {
        s.step <- floor(n/(2.5))
        s <- seq(1, by=s.step, length.out=n)
        s <- s %% n
        s[s==0] <- n
        dup <- which(duplicated(s))[1]
        if (!is.na(dup)) s <- s + 
            rep(0:(s.step-1), each=(dup-1), length.out=n)
    }		
    # 		print(n)
    #  		print(setequal(s, 1:n))
    # 		print(s)
    # 	}
    s
}




treepalette <- function(dat, method="HCL", palette=NULL, palette.HCL.options, prepare.dat=FALSE, ...) {
    #require(colorspace)
    k <- ncol(dat)
	if (method=="HCL") {
		res <- treeapply(dat, list(lb=palette.HCL.options$hue_start, 
                                   ub=palette.HCL.options$hue_end), 
                         fun="addRange", frc=palette.HCL.options$hue_fraction,
                         prepare.dat=prepare.dat)
		
		point <- with(res, (lb+ub)/2)
		chr <- palette.HCL.options$chroma + 
            palette.HCL.options$chroma_slope * (res$l-1)
        #75 - (k-res$l) * 10
		lum <- palette.HCL.options$luminance + 
            palette.HCL.options$luminance_slope * (res$l-1)
        #lum <- 95 - res$l * 10 #90
		color <- hcl(point,c=chr, l=lum)
	} else if (method=="HSV") {
		nl <- nlevels(dat[[1]])
		
        palette <- rep(palette, length.out=nl)

        co <- coords(as(hex2RGB(palette), "HSV"))
        value <- as.list(as.data.frame(co))
		
		res <- treeapply(dat, value, fun="hsvs")
		color <- with(res, hex(HSV(H, S, V)))
	}
	color
}


treeid <- function(dat) {
	current <- apply(dat, MARGIN=1, paste, collapse=".")
	parent <- apply(dat, MARGIN=1, function(x){
		n <- length(x)-sum(is.na(x))-1
		y <- rep(NA, length(x))
		if (n>0) y[1:n] <- x[1:n]
		paste(y,collapse=".")
	})
	list(current=current, parent=parent)
}




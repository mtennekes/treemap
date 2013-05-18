treedepth <- function(data) {
	k <- ncol(data)
	apply(data, MARGIN=1, FUN=function(x)k-sum(is.na(x)))
}

treeapply <- function(dat, values, depth=NULL, fun, ...) {
	if (is.data.table(dat)) {
		dat[, names(dat):=lapply(.SD,as.factor)]
	} else {
		dat <- lapply(dat, as.factor)
		require(data.table)
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
	
	#browser()
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
			dt[ind1==lv, eval(vars):=vls]
		}
	}
	
	#browser()
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


spread <- function(n) {
    ### determine order of brothers/sisters in tree
    ### to prevent that neighbouring brothers/sisters have similar colors, they are spread based on quintiles
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


treepalette <- function(dat, method="HCL", palette=NULL,...) {
	k <- ncol(dat)
	if (method=="HCL") {
		res <- treeapply(dat, list(lb=0, ub=1), fun="addRange", ...)
		
		point <- with(res, (lb+ub)/2)
		chr <- 75 - (k-res$l) * 10 #100
		lum <- 95 - res$l * 10 #90
		color <- hcl(point*360,c=chr, l=lum)
	} else if (method=="HSV") {
		#browser()
		require(colorspace)
		co <- coords(as(hex2RGB(palette), "HSV"))
		nl <- length(unique(dat[[1]])) #nlevels(dat[[1]])
		value <- lapply(as.list(as.data.frame(co)), function(x)x[1:nl])
		
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




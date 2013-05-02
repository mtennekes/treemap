index2col <-
function(dat, position.legend, palette, labels) {
    
    
    
    
    dt <- copy(dat)
    
    depth <- sum(substr(names(dt), 1, 5)=="index")
    
    indexList <- paste0("index", 1:depth)
    
    setkeyv(dt, indexList)
    
    
    ## generic function for hierarchical structures
    ## dt = data.table
    ## index = list of index names
    ## values = named list of initial numeric values (i.e. that belongs to the root node)
    ## function to be applied
    treeapply <- function(dt, index, values, fun) {
        
    }
    
    
    
    
    ########## method 1: split hcl circle

    dt[,lb:=0]
    dt[,ub:=1]
    dt[,LB:=0]
    dt[,UB:=1]
    
    frc <- 0.3
    
    addRange <- function(x) {
        #browser()
        LB <- x$LB[1]
        UB <- x$UB[1]
        
        nr <- nrow(x)
        
        sq <- seq(LB, UB, length.out=nr+1)
        step <- sq[2] - sq[1]
        spacer <- step * frc *.5
        
        list(lb=sq[1:nr]+spacer, ub=sq[2:(nr+1)]-spacer)
    }
    
    
    for (d in 1:depth) {
        if (d==1) {
            dt[dt$l==d, c("lb", "ub"):=addRange(dt[dt$l==d, ])]
            for (lv in levels(dt[[1]])) {
                lowb <- dt$lb[dt[[1]]==lv & dt$l==d]
                upb <- dt$ub[dt[[1]]==lv & dt$l==d]
                dt[dt[[1]]==lv & dt$l>d, c("LB", "UB"):=list(LB=lowb, UB=upb)]
            }
            
        } else {
            id <- indexList[1:(d-1)]
            dtAgg <- dt[dt$l==d, addRange(.SD), by=id]
            for (lv in levels(dt[[(d-1)]])) {
                dt[dt[[(d-1)]]==lv & dt$l==d, c("lb", "ub"):=dtAgg[lv, c("lb", "ub"), with=FALSE]]
            }
        }
    }
    

    require(colorspace)
    browser()
    
    dt[, point:=(lb+ub)/2]
    dt[, color:=hcl(point*360)]
    
    ########## method 2: split hcl circle
    
    
    
    
    
    
    
    co <- coords(as(hex2RGB(palette), "HSV"))
    hex(HSV(co))
    
    
#     
#     
#     addRange
#     
#     levels(dt$index1)
#     
#     
#     id <- indexList[1]
#     dt[, addRange(.SD), by=id]
#     
#     
#     
#     ss <- strsplit(as.character(dat$k), split="__")
#     
#     browser()
#     index1 <- sapply(ss, function(x)x[1])
#     index2 <- sapply(ss, function(x)x[2])
#     
#     index2[index2=="NA"] <- NA
#     #index2 <- as.integer(index2)
#     
#     indexDT <- data.table(index1=index1, index2=index2)
#     
#     
#     condense <- function(x) {
#         y <- order(order(x))
#         y[is.na(x)] <- NA
#         y
#     }
#     indexDT[, index2:=condense(index2), by=index1]
# 
#     
#     color <- palette 
#     colorl <- rep(color, length.out=max(index1))
#     
#     indexDT[, base_color:=colorl[indexDT$index1]]
# 	
#     
#     spread <- function(x) {
#         y <- rep(1, length(x))
#         if (!all(is.na(x))) {
#             y[!is.na(x)] <- seq(0.5, 1.25, length.out=sum(!is.na(x)))[order(order(na.omit(x)))]
#         }
#         y
#     }
#     
#     indexDT[, fact:=spread(index2), by=index1]
#     
#     require(colorspace)
#     createColor <- function(bc, f) {
#         co <- coords(as(hex2RGB(bc), "HSV"))
#         co[, "V"] <- co[, "V"] * f
#         co[, "V"][co[, "V"]>1] <- 1
#         hex(HSV(co))
#     }
#     
# 
#     indexDT[, color:=createColor(base_color, fact)]
#     
#     
 	if (position.legend!="none") {
 	    labels <- dt$n[dt$l==1]
 	    colorl <- dt$color[dt$l==1]
 	    drawLegend(labels, colorl, position.legend=="bottom")
 	}
    
	
	return (dt$color)
}
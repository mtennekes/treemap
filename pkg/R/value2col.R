value2col <-
    function(dat, position.legend, palette, range) {
        if (any(is.na(range))) {
            range <- range(dat$value2)
            if (range[1] > 0) {
                ## all positive
                range[1] <- 0
                prettyP <- pretty(range,n=8)
                palette <- palette[floor(length(palette)/2):length(palette)]
                
            } else if (range[2] < 0) {
                ## all negative
                range[2] <- 0
                prettyP <- pretty(range,n=8)
                palette <- palette[1:ceiling(length(palette)/2)]
            } else {
                ## positive and negative
                prettyP <- pretty(range,n=8)
                
                sumP <- sum(prettyP>0)
                sumN <- sum(prettyP<0)
                
                k <- max(sumP, sumN)
                
                colorTemp <- colorRampPalette(palette, space="rgb")(2*k+1)
                palette <- colorTemp[(k+1-sumN):(k+1+sumP)]
            }
            
        } else {
            prettyP <- pretty(range, n=8)
        }
        color <- colorRampPalette(palette, space="rgb")(99)
        n <- length(prettyP)
        legCol <- colorRampPalette(palette, space="rgb")(n)
        
        minP <- min(prettyP)
        maxP <- max(prettyP)
        
        scale <- floor((dat$value2 - minP) / (maxP - minP) * 98) + 1
        if (any(scale<1)) {
            warning("Values found that are lower than the minimum of range")
            scale[scale<1] <- 1
        }
        if (any(scale>99)) {
            warning("Values found that are higher than the maximum of range")
            scale[scale>99] <- 99
        }
        
        
        if (position.legend!="none") drawLegend(format(prettyP), legCol,
                                                position.legend=="bottom")
        
        return (color[scale])
    }

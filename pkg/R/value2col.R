value2col <-
    function(dat, position.legend, palette, range) {
        maxlev <- max(dat$l)
        
        #browser()
        
        values_all <- dat$c
        values <- values_all[dat$l==maxlev]
        prettyV <- pretty(values, n=8)

        
        if (any(is.na(range))) {
            
            mx <- max(c(values, prettyV))
            
            value.ids <- round((values_all / mx * 50) + 51)
            prettyV.ids <- round((prettyV / mx * 50) + 51)
            
        } else {
            if (any(values < range[1]) || any(values > range[2])) stop("Values are found that exceed the provided range")
            
            prettyV <- prettyV[prettyV>=range[1] & prettyV<=range[2]]
            
            
            diff <- range[2] - range[1]
            value.ids <- round(((values_all + range[1]) /  diff) * 100 + 1)
            prettyV.ids <- round(((prettyV + range[1]) /  diff) * 100 + 1)
            
        }
        
        value.ids[value.ids < 1] <- 1
        value.ids[value.ids > 101] <- 101
        
            
        colpal <- colorRampPalette(palette)(101)
        
        
        if (position.legend!="none") drawLegend(format(prettyV), colpal[prettyV.ids], position.legend=="bottom")
        
        return (colpal[value.ids])
    }

value2col <-
    function(dat, position.legend, palette, range, mapping, border.col, fontfamily.legend, auto.col.mapping, n, format.legend) {
        maxlev <- max(dat$l)
        
 
        values_all <- dat$c
        values <- values_all[dat$l==maxlev]

        if (any(is.na(range))) {
            range <- range(values)
        } else {
            if (any(values < range[1]) || any(values > range[2])) warning("Values are found that exceed the provided range")            
        }
        
        prettyV <- pretty(range, n=n)
        #prettyV <- prettyV[prettyV>=range[1] & prettyV<=range[2]]
        
        # truncate if min (max) value is closer to the second (last) legend value
        
        first <- which.min(abs(prettyV - range[1]))
        last <- which.min(abs(prettyV - range[2]))

        prettyV <- prettyV[first:last]
        
        
        mx <- max(values)
        mn <- min(values)
        m <- mean(c(mx, mn))
        absmx <- max(abs(c(mx, mn)))
        
        if (auto.col.mapping) {
            if (is.na(mapping[1])) mapping[1] <- -absmx
            if (is.na(mapping[2])) mapping[2] <- 0
            if (is.na(mapping[3])) mapping[3] <- absmx
        } else {
            if (is.na(mapping[1])) mapping[1] <- mn
            if (is.na(mapping[2])) mapping[2] <- m
            if (is.na(mapping[3])) mapping[3] <- mx
        }
        
        
        value.ids <- ifelse(values_all < mapping[2],
                            (values_all-mapping[1]) / (mapping[2]-mapping[1]) * 50 + 1,
                            (values_all-mapping[2]) / (mapping[3]-mapping[2]) * 50 + 51
                            )

        prettyV.ids <- ifelse(prettyV < mapping[2],
                              (prettyV-mapping[1]) / (mapping[2]-mapping[1]) * 50 + 1,
                              (prettyV-mapping[2]) / (mapping[3]-mapping[2]) * 50 + 51)
            
            
        value.ids[value.ids < 1] <- 1
        value.ids[value.ids > 101] <- 101

        prettyV.ids[prettyV.ids < 1] <- 1
        prettyV.ids[prettyV.ids > 101] <- 101
        
        colpal <- colorRampPalette(palette)(101)
        args.legend <- format.legend
        args.legend[["x"]] <- prettyV
        legendText <- do.call("format", args.legend)
        if (position.legend!="none") drawLegend(legendText, colpal[prettyV.ids], position.legend=="bottom", border.col, fontfamily.legend)
        
        return (list(colpal[value.ids], range(prettyV), values_all))
    }

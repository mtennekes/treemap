#########################################
### quick example with Gross National Income data
#########################################
data(GNI2010)
treemap(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")

#########################################
### extended examples with fictive business statistics data
#########################################
data(business)

#########################################
### treemap types
#########################################

# index treemap: colors are determined by the index argument
\dontrun{
# large example which takes some time...
treemap(business, 
        index=c("NACE1", "NACE2", "NACE3"), 
        vSize="turnover", 
        type="index")
}
treemap(business[business$NACE1=="C - Manufacturing",],
        index=c("NACE2", "NACE3"),
        vSize=c("employees"),
        type="index")

# value treemap: colors are derived from a numeric variable given by vColor 
# (when omited, all values are set to 1 as in the following example)
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        title.legend="number of NACE4 categories",
        type="value")

# comparisson treemaps: colors indicate change of vSize with respect to vColor
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.prev",
        type="comp")

# density treemaps: colors indicate density (like a population density map)
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="turnover",
        vColor="employees/1000",
        type="dens")

\dontrun{
# depth treemap: show depth
treemap(business,
        index=c("NACE1", "NACE2", "NACE3"), 
        vSize="turnover",
        type="depth")
}

# categorical treemap: colors are determined by a categorical variable
business <- transform(business, data.available = factor(!is.na(turnover)), x = 1)
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="x",
        vColor="data.available",
        type="categorical")

\dontrun{
# color treemap
business$color <- rainbow(nlevels(business$NACE2))[business$NACE2]
treemap(business,
        index=c("NACE1", "NACE2"), 
        vSize="x",
        vColor="color",
        type="color")

# manual
business$color <- rainbow(nlevels(business$NACE2))[business$NACE2]
treemap(business,
        index=c("NACE1", "NACE2"), 
        vSize="turnover",
        vColor="employees",
        type="manual",
        palette=terrain.colors(10))
}

#########################################
### graphical options: control fontsizes
#########################################

\dontrun{
# draw labels of first index at fontsize 12 at the center, 
# and labels of second index at fontsize 8 top left
treemap(business, 
        index=c("NACE1", "NACE2"), 
        vSize="employees", 
        fontsize.labels=c(12, 8), 
        align.labels=list(c("center", "center"), c("left", "top")),
        lowerbound.cex.labels=1)
    
    
# draw all labels at fontsize 12 (only if they fit)
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        fontsize.labels=12,
        lowerbound.cex.labels=1)

# draw all labels at fontsize 12, and if they don't fit, reduce to a minimum of .6*12
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        fontsize.labels=12,
        lowerbound.cex.labels=.6)

# draw all labels at maximal fontsize
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        lowerbound.cex.labels=0,
        inflate.labels = TRUE)

# draw all labels at fixed fontsize, even if they don't fit
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        fontsize.labels=10,
        lowerbound.cex.labels=1,
        force.print.labels=TRUE)

#########################################
### graphical options: color palettes
#########################################

## for comp and value typed treemaps all diverging brewer palettes can be chosen
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.prev",
        type="comp",
        palette="RdBu")

## draw warm-colored index treemap
palette.HCL.options <- list(hue_start=270, hue_end=360+150)
treemap(business, 
        index=c("NACE1", "NACE2"),
        vSize="employees",
        type="index",
        palette.HCL.options=palette.HCL.options)

# terrain colors
business$employees.growth <- business$employees - business$employees.prev
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.growth",
        type="value",
        palette=terrain.colors(10))

# Brewer's Red-White-Grey palette reversed with predefined legend range
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.growth",
        type="value",
        palette="-RdGy",
        range=c(-10000,30000))

# More control over the color palette can be achieved with mapping
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="employees",
        vColor="employees.growth",
        type="value",
        palette="RdYlGn",
        range=c(-10000,30000),           # this is shown in the legend
        mapping=c(-20000, 10000, 40000)) # Rd is mapped to -20k, Yl to 10k, and Gn to 40k
}

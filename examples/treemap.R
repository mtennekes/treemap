#########################################
### quick example
###
### data: Gross national income data
#########################################
data(GNI2010)

# create treemap
tmPlot(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")


#########################################
### extended examples
###
### data: fictive business statistics data
#########################################
data(business)

#########################################
### treemap types
#########################################

# index treemap: colors are determined by the index argument
tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       type="index")

# value treemap: the color variable is directly mapped to the colors
business <- transform(business, x = 1)

tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="x", 
       vColor="x",
       type="value")

# comparisson treemaps: colors indicate change of vSize with respect to vColor
tmPlot(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   vColor="employees.prev",
	   type="comp")

# density treemaps: colors indicate density (like a population density map)
tmPlot(business,
	   index=c("NACE1", "NACE2"), 
	   vSize="turnover",
	   vColor="employees*1000",
	   type="dens")

tmPlot(business,
	   index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
	   vSize="employees",
	   vColor="turnover",
	   type="dens")

# linked treemaps: objects are linked by color over different treemaps
tmPlot(business[business$NACE1=="C - Manufacturing",],
	   index=c("NACE2", "NACE3"),
	   vSize=c("employees"),
	   type="index")

# depth treemap: each aggregation depth has distinct color
tmPlot(business,
	   index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
	   vSize="employees",
	   type="depth")


# categorical treemap: colors are determined by a categorical variable
business$data.available <- factor(!is.na(business$turnover))
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
	   vSize="x", 
	   vColor="data.available",
	   type="categorical")

#########################################
### layout algorithm
#########################################

tmPlot(business, 
	   index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
	   vSize="employees", 
	   algorithm="squarified")

#########################################
### graphical options: fontsize
#########################################

# draw labels at fixed fontsize 12 (if they fit)
tmPlot(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   fontsize.labels=12, 
	   lowerbound.cex.labels=1)

# draw labels at fontsize (.6*12) to 12 (if they fit)
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       fontsize.labels=12, 
	   lowerbound.cex.labels=.6)

# draw all labels at maximal fontsize
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
	   lowerbound.cex.labels=0, 
	   inflate.labels = TRUE)

# draw all labels at fixed fontsize, even if they don't fit
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   force.print.labels=TRUE)

#########################################
### graphical options: color palette
#########################################


## for comp and value typed treemaps all diverging brewer palettes can be chosen
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       vColor="employees.prev",
       type="comp",
       palette="RdBu")

## index treemaps, with palette="HCL":
palette.HCL.options <- list(hue_start=270, hue_end=360+150)
tmPlot(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       type="index",
       palette="HCL",
       palette.HCL.options=palette.HCL.options)


# terrain colors
business$employees.growth <- business$employees - business$employees.prev
tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       vColor="employees.growth", 
	   type="value", 
	   palette=terrain.colors(10))

# Brewer's Red-White-Grey palette reversed with predefined range
tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       vColor="employees.growth", 
	   type="value", 
	   palette="-RdGy", 
	   range=c(-30000,30000))

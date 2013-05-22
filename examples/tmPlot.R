#########################################
### quick example
###
### data: Gross national income data
#########################################

# load Gross national income data
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
### data: fictive structural business statistics data
#########################################

### load fictive structural business statistics data
data(business)

#########################################
### types
#########################################

# index treemap: colors are determined by the index argument
tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       type="index")

# value treemap: the color variable is directly mapped to the colors
tmPlot(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       vColor="employees.growth",
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
	   index=c("NACE1", "NACE2"), 
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
business$rand <- cut(runif(nrow(business)), breaks=10)
tmPlot(business[business$NACE1=="C - Manufacturing",], 
	   index=c("NACE3", "NACE4"), 
	   vSize="employees", 
	   vColor="rand",
	   type="categorical")

#########################################
### layout algorithm
#########################################

tmPlot(business, 
	   index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
	   vSize="employees", 
	   vColor="employees.growth",
	   type="value",
	   algorithm="squarified")

#########################################
### graphical options: fontsize
#########################################

# draw labels at fixed fontsize (fit only)
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="comp",
	   fontsize.labels=12, 
	   lowerbound.cex.labels=1)

# draw labels at flexible fontsize (skip tiny rectangles)
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   type="comp",
	   fontsize.labels=12, 
	   lowerbound.cex.labels=.6)

# draw labels at maximal fontsize
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   type="comp",
	   fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   inflate.labels = TRUE)

# draw all labels at fixed fontsize
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   type="comp",
	   fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   force.print.labels=TRUE)

#########################################
### graphical options: color palette
#########################################

# terrain colors
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value", 
	   palette=terrain.colors(10))

# Brewer's Red-White-Grey palette reversed with predefined range
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value", 
	   palette="-RdGy", 
	   range=c(-20000,20000))

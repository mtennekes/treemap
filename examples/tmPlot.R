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
data(sbsData)
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08


#########################################
### types
#########################################

# value treemap: the color variable is directly mapped to the colors
tmPlot(sbsData, 
       index=c("section", "subsection"), 
       vSize="employees09", 
       vColor="employees.growth",
       type="value")

# comparisson treemaps: colors indicate change of vSize with respect to vColor
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="comp")

# four comparisson treemaps
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize=c("employees09", "value added09", "turnover09", "salaries09"), 
	   vColor=c("employees08", "value added08", "turnover08", "salaries08"),
	   type="comp")

# density treemaps: colors indicate density (like a population density map)
tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="turnover09",
	   vColor="employees09*1000",
	   type="dens")

tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="employees09",
	   vColor="turnover09",
	   type="dens")

# linked treemaps: objects are linked by color over different treemaps
tmPlot(sbsData[sbsData$section=="Manufacturing",],
	   index="subsection",
	   vSize=c("income09", "employees09", "expenditures09", "salaries09"),
	   type="linked")

# index treemap: each aggregation index has distinct color
tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="employees09",
	   type="index")

# categorical treemap: colors are determined by a categorical variable
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="section",
	   type="categorical")

#########################################
### layout algorithm
#########################################

tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
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

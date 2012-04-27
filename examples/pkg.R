# fictive structural business statistics (sbs) data of 2008 and 2009
data(sbsData)

# comparisson treemap: colors indicate change of vSize with respect to vColor
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="comp")

# density treemap: colors indicate density (like a population density map)
tmPlot(sbsData,
	   index=c("section", "subsection"),
	   vSize="turnover09",
	   vColor="employees09*1000",
	   type="dens")

# value treemap: the color variable is directly mapped to the colors
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value")

# categorical treemap: colors are determined by a categorical variable
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="section",
	   type="categorical")
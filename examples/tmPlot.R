
### fictive structural business statistics (sbs) data of 2008 and 2009
data(sbsData)


### treemap examples

# comparisson treemaps: colors indicate change of vSize with respect to vColor
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08")

tmPlot(sbsData, 
	   index=c("section"), 
	   vSize="employees09", 
	   vColor="employees08")

# four comparisson treemaps
tmPlot(sbsData, 
	   index="section", 
	   vSize="employees09+value added09+turnover09+salaries09", 
	   vColor="employees08+value added08+turnover08+salaries08")

# density treemaps: colors indicate density (like a population density map)
tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="turnover09",
	   vColor="employees09/1000*turnover09")

tmPlot(sbsData,
	   index=c("section", "subsection"), 
	   vSize="employees09",
	   vColor="turnover09/employees09")

# linked treemaps: objects are linked by color over different treemaps
tmPlot(sbsData[sbsData$section=="Manufacturing",],
	   index="subsection",
	   vSize="income09+employees09+expenditures09+salaries09",
	   vColor="")

# value treemap (aka Map of the Market)
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth",
	   type="value")


### graphical options: fontsize

# draw labels at fixed fontsize (fit only)
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   fontsize.labels=12, 
	   lowerbound.cex.labels=1)

# draw labels at flexible fontsize (skip tiny rectangles)
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   fontsize.labels=12, 
	   lowerbound.cex.labels=.6)

# draw labels at maximal fontsize
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   inflate.labels = TRUE)

# draw all labels at fixed fontsize
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08", 
	   fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   force.print.labels=TRUE)


### graphical options: color palette

# terrain colors
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value", 
	   palette=terrain.colors(10))

# Brewer's Red-White-Blue palette with predefined range
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   type="value", 
	   palette="RdBu", 
	   vColorRange=c(-20000,20000))

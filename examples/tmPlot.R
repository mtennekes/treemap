# fictive structural business statistics (sbs) data of 2008 and 2009
data(sbsData)

# comparisson treemaps
tmPlot(sbsData, index=c("section", "subsection"), vSize="employees09", vColor="employees08", sortID="-size")

tmPlot(sbsData, index=c("section"), vSize="employees09", vColor="employees08", sortID="-size")


# four comparisson treemaps
tmPlot(sbsData, 
	   index="section", 
	   vSize="employees09+value added09+turnover09+salaries09",
	   vColor="employees08+value added08+turnover08+salaries08", 
	   sortID="-size")


# density treemaps
tmPlot(sbsData,
	   index=c("section", "subsection"),
	   vSize="turnover09",
	   vColor="employees09/1000*turnover09", 
	   sortID="-size")

tmPlot(sbsData,
	   index=c("section", "subsection"),
	   vSize="employees09",
	   vColor="turnover09/employees09", 
	   sortID="-size", 
	   palette=terrain.colors(8))


# linked treemaps
tmPlot(sbsData[sbsData$section=="Manufacturing",],
	   index="subsection",
	   vSize="income09+employees09+expenditures09+salaries09",
	   vColor="", 
	   sortID="-size")


# value treemap (aka Map of the Market)
sbsData$employees.growth <- sbsData$employees09 - sbsData$employees08
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees.growth", 
	   sortID="-size", 
	   type="value", 
	   palette="RdBu")

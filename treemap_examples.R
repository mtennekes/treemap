library(treemap)

library(devtools)
load_all("pkg")

tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="index")

load("ps0607_final.Rdata")


tmPlot(ps07final, index=c("sector", "subsector"), vSize="value_added", vColor="", type="fixed")

tmPlot(ps07final, index=c("sector", "subsector"), vSize="employees", vColor="turnover", na.rm=TRUE)

tmPlot(ps07final, index=c("sector", "subsector", "SBI3d"), vSize=c("employees", "turnover"), vColor=c("turnover", "employees"), type="linked", na.rm=TRUE)

tmPlot(ps07final, index=c("sector", "subsector", "SBI3d"), 
	   vSize=c("employees", "turnover"), 
	   vColor=c("turnover", "employees/1000"), type="dens", na.rm=TRUE)



levels(ps07final$subsector)[1] <- "X"

tmPlot(ps07final[ps07final$sector=="Manufacturing", ], index=c("subsector", "subsubsector"), vSize="employees", vColor="turnover/employees")

library(treemap)

library(devtools)
load_all("pkg")

tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   fontsize.labels=9)

load("ps07final_SBI3test.Rdata")


tmPlot(ps07final, index=c("sector", "subsector"), vSize="value_added", vColor="")

tmPlot(ps07final, index=c("sector", "subsector"), vSize="employees", vColor="turnover/employees")


tmPlot(ps07final, index=c("sector", "subsector", "subsubsector"), vSize=c("employees", "value_added"), vColor=c("turnover", "purchases"), na.rm=TRUE)

tmPlot(ps07final, index=c("sector", "subsector"), 
	   vSize=c("employees", "turnover"), 
	   vColor=c("turnover", "employees/1000"), type="dens")



levels(ps07final$subsector)[1] <- "X"

tmPlot(ps07final[ps07final$sector=="Manufacturing", ], index=c("subsector", "subsubsector"), vSize="employees", vColor="turnover/employees")

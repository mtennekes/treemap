library(treemap)

library(devtools)
load_all("pkg")

tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="index")

load("ps0607_final.Rdata")



value <- c(6,6,4,3,2,2,1)
rec <- c(0,0, 6, 4)

squarified(value, rec)




tmPlot(ps07final, index=c("sector", "subsector"), vSize="value_added", type="linked")

tmPlot(ps07final, index=c("sector", "subsector"), vSize="employees", vColor="turnover", na.rm=TRUE)

tmPlot(ps07final, index=c("sector", "subsector", "SBI3d"), vSize=c("employees", "turnover"), vColor=c("turnover", "employees"), type="dens", na.rm=TRUE, algorithm="squarified")

tmPlot(ps07final, index=c("sector", "subsector", "SBI3d"), 
	   vSize=c("turnover", "turnover"), 
	   vColor=c("employees", "employees/1000"), type="dens", palette="-RdYlBu", na.rm=TRUE)



levels(ps07final$subsector)[1] <- "X"

tmPlot(ps07final[ps07final$sector=="Manufacturing", ], index=c("subsector", "subsubsector"), vSize="employees", vColor="turnover/employees")


library(portfolio)
data(dow.jan.2005)
tmPlot(dow.jan.2005, 
	   index=c("sector", "symbol"), 
	   vSize="price", 
	   vColor="month.ret",
	   algorithm="squarified",
	   palette="RdYlBu")


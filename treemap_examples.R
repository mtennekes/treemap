library(treemap)

library(devtools)
load_all("pkg")

tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="index")




value <- c(6,6,4,3,2,2,1)
rec <- c(0,0, 6, 4)

squarified(value, rec)


## load final ps 06 and 07 data, apply weights and merge them
load("ps0607_final.Rdata")

ps06final[,8:22] <- lapply(ps06final[,8:22], FUN="*", ps06final$weight)
ps06final$weight <- NULL

ps07final[,11:25] <- lapply(ps07final[,11:25], FUN="*", ps07final$weight)
ps07final$weight <- NULL


ps6f <- aggregate(ps06final[,7:21], by=list(sector=ps06final$sector, subsector=ps06final$subsector, SBI3d=ps06final$SBI3d, SBI=ps06final$SBI), FUN=function(x)sum(as.numeric(x), na.rm=TRUE))

ps7f <- aggregate(ps07final[,10:24], by=list(sector=ps07final$sector, subsector=ps07final$subsector, SBI3d=ps07final$SBI3d, SBI=ps07final$SBI), FUN=function(x)sum(as.numeric(x), na.rm=TRUE))

psfinal <- merge(ps6f, ps7f, by=c("sector", "subsector", "SBI3d", "SBI"),suffixes = c(".2006",".2007"))

## data editing workshop

tmPlot(psfinal, vSize="value_added.2007", vColor="value_added.2006", index=c("sector", "subsector"), type="comp", 
	   fontsize.labels=c(12,8))

tmPlot(subset(psfinal,subset=sector=="Manufacturing"), vSize="value_added.2007", vColor="value_added.2006", index=c("subsector", "SBI3d"), type="comp", 
	   fontsize.labels=c(12,10))


tmPlot(subset(psfinal,subset=subsector=="Electrical and optical equipment"), vSize="value_added.2007", vColor="value_added.2006", index=c("SBI3d", "SBI"), type="comp", 
	   fontsize.labels=c(12,10))


tmPlot(psfinal, vSize="value_added.2007", vColor="value_added.2006", index=c("sector", "subsector", "SBI3d", "SBI"), type="comp", 
	   fontsize.labels=c(12,10))





tmPlot(psfinal, vSize="turnover.2007", vColor="employees.2007/1000", index=c("sector", "subsector", "SBI3d"), type="dens", 
	   fontsize.labels=8)

#zoom in
tmPlot(psfinal[psfinal$sector=="Manufacturing",], vSize="turnover.2007", vColor="employees.2007/1000", index=c("subsector", "SBI3d"), type="dens", 
	   fontsize.labels=8)


tmPlot(psfinal, vSize="employees.2007", vColor="turnover.2007*1000", index=c("sector", "subsector", "SBI3d"), type="dens", 
	   fontsize.labels=c(12,10,8))






tmPlot(ps07final, index=c(""))



tmPlot(ps07final, index=c("sector", "subsector"), vSize="value_added", type="linked")

tmPlot(ps07final, index=c("sector", "subsector"), vSize="employees", vColor="turnover", na.rm=TRUE)

tmPlot(ps07final, index=c("sector", "subsector", "SBI3d"), vSize=c("employees", "turnover"), vColor=c("turnover", "employees"), type="dens", na.rm=TRUE, algorithm="squarified")

tmPlot(ps07final, index=c("sector", "subsector", "SBI3d"), 
	   vSize=c("turnover", "turnover"), 
	   vColor=c("employees", "employees/100"), type="value", palette="-RdYlBu", na.rm=TRUE)



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


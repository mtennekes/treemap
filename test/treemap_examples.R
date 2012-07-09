

data(sbsData)
tm <- tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize=c("employees09", "turnover09"), 
	   vColor=c("turnover09", "employees09"),
	   type="linked")

tm

npcClick <- tmClick()

# locate clicked object
print(tmLocate(npcClick, tm))



value <- c(6,6,4,3,2,2,1)
rec <- c(0,0, 6, 4)

squarified(value, rec)







library(portfolio)
data(dow.jan.2005)
map.market(id    = dow.jan.2005$symbol,
		   area  = dow.jan.2005$price,
		   group = dow.jan.2005$sector,
		   color = 100 * dow.jan.2005$month.ret)

tmPlot(dow.jan.2005, 
	   index=c("sector", "symbol"), 
	   vSize="price", 
	   vColor="month.ret*100",
	   algorithm="squarified",
	   palette="RdYlBu",
	   subtitle = "month.ret (in percentages)",
	   vColorRange=c(-13,13),
	   algorithm="squarified")


data(sbsData)


### treemap examples
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="employees08",
	   type="comp",
	   algorithm="squarified",
	   fontsize.labels=14
	   )

sbsData <- transform(sbsData, temp =
	(employees09 - employees08) / employees08 * 100)
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="temp",
	   type="value",
	   algorithm="squarified",
	   fontsize.labels=14,
	   vColorRange=c(-80,80)
	   )


tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="temp",
	   type="value",
	   algorithm="squarified",
	   fontsize.labels=14
	   )

### test type==categorical
sbsData$cat <- factor(rep(1:4, length.out=25),labels=letters[1:4])

load_all("pkg")
tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize="employees09", 
	   vColor="section",
	   type="categorical")


sbsData$employees08[2] <- 0
sbsData$employees09[14] <- 0.0001
load_all("pkg")
tm <- tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize=c("employees09","turnover09"), 
	   vColor=c("employees08","turnover08"),
	   type="value",
	   position.legend="bottom",
	   bg.labels="#CCCCCCAA")

clc <- tmClick()
tmLocate(clc, tm)

tm <- tmPlot(sbsData, 
	   index=c("section", "subsection"), 
	   vSize=c("employees09", "turnover09", "purchases09"), 
	   vColor=rep("section",3),
	   type="categorical",
	   position.legend="right",
	   fontsize.legend=8,
	   bg.labels="#CCCCCCAA")


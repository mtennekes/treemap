library(treemap)

library(devtools)
load_all("pkg")


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


pdf(file="tm_comp1.pdf", width=7, height=6)
tm1 <- tmPlot(psfinal, 
	   vSize="value_added.2007", 
	   vColor="value_added.2006", 
	   index=c("sector", "subsector"), 
	   type="comp", 
	   title="Total value added",
	   subtitle="Growth w.r.t. last year",
	   fontsize.labels=c(10,8),
	   aspRatio=1.3)
dev.off()

## determine aspRatio rectangle manufacturing
asp <- (sum(tm1[[1]][[1]]$w[c(4, 8, 9)]) / 
	sum(tm1[[1]][[1]]$h[c(4, 10, 12)])) * 1.3



pdf(file="tm_comp2.pdf", width=7, height=6) #7,6
tm2 <- tmPlot(subset(psfinal,subset=sector=="Manufacturing"), 
	   vSize="value_added.2007", 
	   vColor="value_added.2006", 
	   index=c("subsector"),#, "SBI3d"), 
	   type="comp", 
	  title="Total value added in the sector Manufacturing",
	   subtitle="Growth w.r.t. last year",
	   fontsize.labels=c(10,8),
	   aspRatio=asp)
dev.off()

# pdf(file="tm_comp3.pdf", width=7, height=6)
# tmPlot(subset(psfinal,subset=subsector=="Machinery and equipment n.e.c."), 
# 	   vSize="value_added.2007", 
# 	   vColor="value_added.2006", 
# 	   index=c("SBI3d", "SBI"), 
# 	   type="comp", 
# 	   title="Total value added in the subsector Machinery and equipment n.e.c.",
# 	   subtitle="Growth w.r.t. last year",
# 	   fontsize.labels=c(10,8))
# dev.off()



pdf(file="tm_dens.pdf", width=7, height=6) #7,6
	tmPlot(subset(psfinal,subset=sector=="Manufacturing"), 
		   vSize="employees.2007", 
		   vColor="turnover.2007*1000", 
		   index=c("subsector", "SBI3d"), 
		   type="dens",
		   title="Number of persons employed in the sector Manufacturing",
		   subtitle="Turnover (in millions) per person employed",
		   fontsize.labels=c(10,8), lowerbound.cex.labels=0.3)
dev.off()

#zoom in
tmPlot(psfinal[psfinal$sector=="Manufacturing",], vSize="turnover.2007", vColor="employees.2007/1000", index=c("subsector", "SBI3d"), type="dens", 
	   fontsize.labels=8)


tmPlot(psfinal, vSize="employees.2007", vColor="turnover.2007*1000", index=c("sector", "subsector", "SBI3d"), type="dens", 
	   fontsize.labels=c(12,10,8))




system.time({
	tmPlot(ps07final, index=c("sector", "subsector", "SBI3d"), 
	   vSize=c("employees", "turnover"), 
	   vColor=c("turnover", "employees"), 
	   type="dens", na.rm=TRUE, algorithm="squarified")
})



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
	   vColorRange=c(-13,13))



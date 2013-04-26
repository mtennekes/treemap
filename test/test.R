data(sbsData)

sbsData$ssubsection <- sbsData$subsection

str(sbsData)

dtf <- sbsData
index <- c("section", "subsection", "ssubsection")
vSize <- "turnover08"
vColor <- "employees08"


data(GNI2010)

# create treemap
tmPlot(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")

tmPlot(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="index")
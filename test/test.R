data(sbsData)

sbsData$ssubsection <- sbsData$subsection

str(sbsData)


tmPlot(sbsData,
       index=c("section", "subsection"),
       vSize="turnover08",
       vColor="section",
       type="categorical",
       palette="HCL")


tmPlot(sbsData,
       index=c("section", "subsection"),
       vSize="turnover08",
       type="index",
       palette="HCL")


tmPlot(sbsData,
       index=c("section", "subsection"),
       vSize="turnover08",
       type="depth",
       palette="HCL")


tmPlot(sbsData,
       index=c("section", "subsection", "ssubsection"),
       vSize="turnover08",
       type="index",
       palette="HCL")

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
       type="index")


GNI2010$col <- "#ABCDEF"

tmPlot(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="col",
       type="color")

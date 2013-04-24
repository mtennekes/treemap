data(sbsData)

sbsData$ssubsection <- sbsData$subsection

str(sbsData)

dtf <- sbsData
index <- c("section", "subsection", "ssubsection")
vSize <- "turnover08"
vColor <- "employees08"

tmPlot(sbsData, 
       index=c("section", "subsection", "ssubsection"), 
       vSize="employees09", 
       vColor="employees08",
       type="comp",
       algorithm="squarified",
       fontsize.labels=14
)


data(GNI2010)

# create treemap
tmPlot(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")
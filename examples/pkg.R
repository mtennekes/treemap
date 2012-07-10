# load Gross national income data
data(GNI2010)

# create treemap
tmPlot(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")

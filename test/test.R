data(business)

tmPlot(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="employees")


palette.HCL.options <- list(hue_start=30, hue_end=390, hue_spread=TRUE,
                        hue_fraction=0.5, chroma=60, luminance=70, 
                        chroma_slope=5, luminance_slope=-10)


tmPlot(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="employees",
       palette.HCL.options=palette.HCL.options, bg.labels=255)

tmPlot(business, index=c("NACE1"), vSize="employees",
       palette.HCL.options=palette.HCL.options, bg.labels=255)

tmPlot(business, index=c("NACE1", "NACE2"), vSize="employees",
       palette.HCL.options=palette.HCL.options, bg.labels=255)

tmPlot(business, index=c("NACE1", "NACE2", "NACE3"), vSize="employees",
       palette.HCL.options=palette.HCL.options, bg.labels=255)


data(GNI2010)

# create treemap
tm <- tmPlot(GNI2010,
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

data(business)



Rprof(tmp <- tempfile())
treemap(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="employees")
Rprof()
summaryRprof(tmp)
unlink(tmp)

palette.HCL.options <- list(hue_start=30, hue_end=390, hue_spread=TRUE,
                        hue_fraction=0.5, chroma=60, luminance=70, 
                        chroma_slope=5, luminance_slope=-10)


treemap(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="employees",
       palette.HCL.options=palette.HCL.options, bg.labels=255)


treepalette(business[, c("NACE1", "NACE2", "NACE3", "NACE4")],
        palette.HCL.options=palette.HCL.options,
        return.parameters=TRUE)

treemap(business, index=c("NACE1", "NACE2"), vSize="employees",
        palette.HCL.options=palette.HCL.options, bg.labels=255)


treegraph(business[, c("NACE1", "NACE2", "NACE3", "NACE4")],
          palette.HCL.options=palette.HCL.options)

treegraph(business[, c("NACE1", "NACE2")],
          palette.HCL.options=palette.HCL.options,
          show.labels=TRUE)

data(GNI2010)

treegraph(GNI2010[, c("continent", "iso3")],show.labels=TRUE,
          palette.HCL.options=palette.HCL.options)

treemap(GNI2010, index=c("continent", "iso3"), vSize="population",
        palette.HCL.options=palette.HCL.options, bg.labels=255)


treegraph(GNI2010[, c("continent", "iso3")], vertex.label.dist=.2, 
          palette.HCL.options=palette.HCL.options,
          show.labels=TRUE,
          stack.labels=FALSE)



test <- treepalette(business[, c("NACE1", "NACE2")],
            palette.HCL.options=palette.HCL.options,
            return.parameters=TRUE)


treemap(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="employees",
        palette=rainbow(8),
        bg.labels=255)


treemap(business, index=c("NACE1"), vSize="employees",
       palette.HCL.options=palette.HCL.options, bg.labels=255)

treemap(business, index=c("NACE1", "NACE2"), vSize="employees",
       palette.HCL.options=palette.HCL.options, bg.labels=255)

treemap(business, index=c("NACE1", "NACE2", "NACE3"), vSize="employees",
       palette.HCL.options=palette.HCL.options, bg.labels=255)


data(GNI2010)

# create treemap
tm <- treemap(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")

treemap(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       type="index")


treemap(GNI2010,
        index=c("continent", "iso3"),
        vSize="population",
        type="index",
        lwds=c(5,4,3))


GNI2010$col <- "#ABCDEF"

treemap(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="col",
       type="color")

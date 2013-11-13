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


library(ggplot2)
data(diamonds)


treemap(diamonds, index=c("carat", "color"), vSize="price")

treemap(diamonds, index=c("cut", "color"), vSize="price")
treegraph(diamonds, index=c("cut", "color"), show.labels=TRUE)
treepalette(diamonds, index=c("cut", "color"))


# create windows font mappings
windowsFonts(
    A=windowsFont("Arial Black"),
    B=windowsFont("Bookman Old Style"),
    C=windowsFont("Comic Sans MS"),
    D=windowsFont("Symbol")
)
tm <- treemap(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value",
       fontfamily.title="C",
       fontfamily.labels="C",
       fontfamily.legend="C",
              fontface.labels=c("bold", "italic"))

treemap(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       type="index")


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



data(GNI2010)
treemap(GNI2010,
        index=c("continent", "iso3"),
        vSize="population",
        type="index")
data(business)
treemap(business, index=c("NACE1", "NACE2"), vSize="employees")
treemap(business[as.integer(business$NACE1)==2,], index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="employees", 
        align.labels=list(c("center", "center"), c("center", "bottom")), border.col="blue",
        fontsize.labels=c(40,30,20,10))

treemap(business[as.integer(business$NACE1)==2,], index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="employees", 
        align.labels=list(c("center", "center"), c("center", "bottom")), border.col="blue",
        fontsize.labels=c(40,35,30,25))

treemap(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="employees", 
        align.labels=list(c("center", "center"), c("center", "bottom")), border.col=c("blue", "yellow", "red", "green"),
        fontsize.labels=c(40,35,30,25))

tm <- treemap(business, index=c("NACE1", "NACE2", "NACE3"), vSize="employees")
tm <- treemap(business, index=c("NACE1", "NACE2", "NACE3"), vSize="employees", vColor="employees.prev", type="comp", range=c(-100, 2000))


tm <- treemap(business[business$NACE1=="C - Manufacturing" & business$NACE2=="19 - Manufacture of coke and refined petroleum products",], index=c("NACE2"), vSize="turnover", vColor="turnover.prev", type="comp", range=c(-80, 100))

tm <- treemap(business[business$NACE1=="C - Manufacturing",], index=c("NACE2"), vSize="turnover", vColor="turnover", type="value", range=c(0, 5.5e7))

tm <- treemap(business[business$NACE1=="C - Manufacturing",], index=c("NACE2"), vSize="turnover", vColor="turnover", type="manual", range=c(0, 5.5e7), palette="RdYlGn")

itreemap(business)

itreemap()


tm <- treemap(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), vSize="turnover", vColor="turnover.prev", type="comp")
tm <- treemap(business, index=c("NACE1"), vSize="turnover", vColor="turnover.prev", type="comp")


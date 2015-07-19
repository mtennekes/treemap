devtools::install_github("gluc/data.tree")
devtools::install_github('ramnathv/htmlwidgets')
devtools::install_github('jeroenooms/jsonlite')

library(d3treeR)
library(treemap)

data(GNI2010)
d3tree(
    treemap(
        GNI2010,
        index=c("continent", "iso3"),
        vSize="population",
        vColor="GNI",
        type="value"
    )
    ,rootname = "World"
)

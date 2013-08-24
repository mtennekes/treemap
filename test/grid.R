require(grid)
grid.newpage()

data(business)
numVars <- c("turnover", "turnover.prev", "employees", "employees.prev")

pushViewport(viewport(layout=grid.layout(2, 2, widths=c(0.4,0.6), heights=c(0.7,0.3))))

for (i in 1:2) {
    for (j in 1:2) {
        vp <- viewport(layout.pos.col=i, layout.pos.row=j)
        treemap(business, index=c("NACE1", "NACE2"), vSize=numVars[i+(j-1)*2],
                type="index",vp=vp)
    }
}

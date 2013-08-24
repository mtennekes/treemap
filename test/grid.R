
library(grid)
grid.newpage()
grid.rect()
data(business)
do_treemap <- function(ind){
    vp <- viewport(layout.pos.col=1, layout.pos.row=ind)
    #pushViewport(vp)
    treemap(business, index=c("NACE1"), vSize="turnover", type="index",vp=vp)
    #popViewport()
    #popViewport() #treemap doees not seem to pop corretly
    #popViewport() #and one more!
}

lapply(1:3, do_treemap)


grid.newpage()
grid.rect()
pushViewport(viewport(layout=grid.layout(3, 1), name="3_1_grid"))

vp <- viewport(layout.pos.col=1, layout.pos.row=1, name="row1")
treemap(business, index=c("NACE1"), vSize="turnover", type="index",vp=vp)

vp <- viewport(layout.pos.col=1, layout.pos.row=2)
treemap(business, index=c("NACE1"), vSize="employees", type="index",vp=vp)

vp <- viewport(layout.pos.col=1, layout.pos.row=3)
treemap(business, index=c("NACE1"), vSize="turnover", type="index",vp=vp)




tp <- treemap(business, index=c("NACE1", "NACE2", "NACE3"), vSize="turnover", type="index")
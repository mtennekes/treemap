data(business)

#treegraph(business, index=c("NACE1", "NACE2", "NACE3", "NACE4"), show.labels=FALSE, vertex.layout=igraph::layout.auto)

## mooi voorbeeld voor in paper (meerdere evenwichten)
treegraph(business, index=c("NACE1", "NACE2", "NACE3"), show.labels=FALSE, vertex.layout=igraph::layout.fruchterman.reingold)

treegraph(business, index=c("NACE1", "NACE2", "NACE3"), show.labels=FALSE, vertex.layout=igraph::layout.auto)


set.seed(20140110)
l <- paste("Category", LETTERS[1:26])
l <- sample(paste("Category", paste0(rep(LETTERS[1:26], each=26), 
                              rep(LETTERS[1:26], times=26))), 100)

levs <- c(6, 20) # number of 1st and 2nd hierarchical categories

h1 <- sample(l, levs[1])
h2 <- sample(setdiff(l, h1), levs[2])
h3 <- setdiff(l, c(h1,h2))

dat <- data.frame(h1=factor(NA, levels=h1),
                  h2=factor(sample(h2, length(h3), replace=TRUE), levels=h2),
                  h3=factor(h3, levels=h3),
                  value = rnorm(length(h3), mean=100, sd=30))

h2parents <- sample(h1, length(h2), replace=TRUE)
dat$h1 <- h2parents[match(dat$h2, h2)]


pdf(file="../visweek2014/tm1hcp.pdf", width=9, height=7)
treemap(dat, index=c("h1", "h2", "h3"), vSize="value", title="")  
dev.off()


pdf(file="../visweek2014/tm1ref.pdf", width=9, height=7)
treemap(dat, index=c("h1", "h2", "h3"), vSize="value", vColor="h1", type="categorical", position.legend="none", palette="Set1", title="")  
dev.off()





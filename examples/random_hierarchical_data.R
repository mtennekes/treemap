d <- random.hierarchical.data(200)
treemap(d, index=names(d)[1:(ncol(d)-1)], vSize="x")

d <- random.hierarchical.data(number.children=5)
treemap(d, index=names(d)[1:(ncol(d)-1)], vSize="x")

d <- random.hierarchical.data(method="full.tree", number.children=3, value.generator=runif)
treemap(d, index=names(d)[1:(ncol(d)-1)], vSize="x")

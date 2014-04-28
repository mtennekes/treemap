dat <- data.frame(ind1 = factor(c("PC1", "PC1", "PC2", "PC2")), 
                  ind2 = factor(c("paste('x(', mu, ', ', sigma, ')')", "alpha", "paste(frac(1, sigma*sqrt(2*pi)), plain(e)^{frac(-(x-mu)^2, 2*sigma^2)}, sep='')", "theta")),
                  size = c(3,4,5,6))

treemap(dat, index=c("ind1", "ind2"), vColor="ind1", vSize="size", type="categorical", position.legend="bottom", fontsize.labels=c(0, 12), title="", title.legend="", eval.labels=TRUE)


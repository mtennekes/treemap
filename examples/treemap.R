#########################################
### quick example
###
### data: Gross national income data
#########################################
data(GNI2010)

# create treemap
treemap(GNI2010,
       index=c("continent", "iso3"),
       vSize="population",
       vColor="GNI",
       type="value")


#########################################
### extended examples
###
### data: fictive business statistics data
#########################################
data(business)

#########################################
### treemap types
#########################################

# index treemap: colors are determined by the index argument
treemap(business, 
       index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
       vSize="employees", 
       type="index")

# value treemap: the color variable is directly mapped to the colors
treemap(business, 
       index=c("NACE1", "NACE2", "NACE3"), 
       vSize="employees", 
       vColor="employees",
       type="value")

# comparisson treemaps: colors indicate change of vSize with respect to vColor
treemap(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   vColor="employees.prev",
	   type="comp")

# density treemaps: colors indicate density (like a population density map)
treemap(business,
	   index=c("NACE1", "NACE2"), 
	   vSize="turnover",
	   vColor="employees*1000",
	   type="dens")

treemap(business,
	   index=c("NACE1", "NACE2"), 
	   vSize="employees",
	   vColor="turnover",
	   type="dens")

# linked treemaps: objects are linked by color over different treemaps
treemap(business[business$NACE1=="C - Manufacturing",],
	   index=c("NACE2", "NACE3"),
	   vSize=c("employees"),
	   type="index")

# depth treemap: each aggregation depth has distinct color
treemap(business,
	   index=c("NACE1", "NACE2", "NACE3", "NACE4"), 
	   vSize="employees",
	   type="depth")


# categorical treemap: colors are determined by a categorical variable
business <- transform(business, data.available = factor(!is.na(turnover)), x = 1)
treemap(business, 
       index=c("NACE1", "NACE2"), 
	   vSize="x", 
	   vColor="data.available",
	   type="categorical")

#########################################
### layout algorithm
#########################################

treemap(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   algorithm="squarified")

#########################################
### graphical options: fontsize
#########################################

# draw labels at fixed fontsize 12 (if they fit)
treemap(business, 
	   index=c("NACE1", "NACE2"), 
	   vSize="employees", 
	   fontsize.labels=12, 
	   lowerbound.cex.labels=1)

# draw labels at fontsize (.6*12) to 12 (if they fit)
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       fontsize.labels=12, 
	   lowerbound.cex.labels=.6)

# draw all labels at maximal fontsize
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
	   lowerbound.cex.labels=0, 
	   inflate.labels = TRUE)

# draw all labels at fixed fontsize, even if they don't fit
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       fontsize.labels=10, 
	   lowerbound.cex.labels=1, 
	   force.print.labels=TRUE)

#########################################
### graphical options: color palette
#########################################


## for comp and value typed treemaps all diverging brewer palettes can be chosen
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       vColor="employees.prev",
       type="comp",
       palette="RdBu")

## index treemaps, with palette="HCL":
palette.HCL.options <- list(hue_start=270, hue_end=360+150)
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       type="index",
       palette="HCL",
       palette.HCL.options=palette.HCL.options)


# terrain colors
business$employees.growth <- business$employees - business$employees.prev
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       vColor="employees.growth", 
	   type="value", 
	   palette=terrain.colors(10))

# Brewer's Red-White-Grey palette reversed with predefined range
treemap(business, 
       index=c("NACE1", "NACE2"), 
       vSize="employees", 
       vColor="employees.growth", 
	   type="value", 
	   palette="-RdGy", 
	   range=c(-30000,30000))

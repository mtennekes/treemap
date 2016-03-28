library(tmap)

## load world country data

download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries_lakes.zip", "./build/ne_50m_admin_0_countries_lakes.zip")
unzip("./build/ne_50m_admin_0_countries_lakes.zip", exdir="./build")
world50 <- read_shape("./build/ne_50m_admin_0_countries_lakes.shp")

## load worlbank GNI data
#http://data.worldbank.org/data-catalog/GNI-per-capita-Atlas-and-PPP-table


x <- read.csv("build/GNIPC.csv", skip=7, header = FALSE, stringsAsFactors = FALSE)

x <- x[!is.na(x[[1]]) & !x[[4]]=="" & !x[[5]] %in% c("", "..", letters), c(1,4,5)]
x <- x[1:(which(x[[1]]=="WLD")-1), ]
names(x) <- c("iso3", "country", "GNI")
x$GNI <- as.integer(gsub(",", "", x$GNI, fixed=TRUE))
x[!x$iso3 %in% world50$iso_a3,]
x$iso3[x$iso3=="ROM"] <- "ROU"
x$iso3[x$iso3=="ZAR"] <- "COD"

GNI2014 <- cbind(x, world50@data[match(x$iso3, world50$iso_a3), c("continent", "pop_est")])[c("iso3", "country", "continent", "pop_est", "GNI")]
names(GNI2014)[4] <- "population"

ids <- which(!GNI2014$iso3 %in% world50$iso_a3)
GNI2014[ids, ]

# retrieved from Wiki on 2015-03-25
GNI2014$continent[ids] <- c("Europe", "Oceania", "Europe", "Asia",  "Asia")
GNI2014$population[ids] <- c(79218  ,  10640 ,   1859203,  4550368 , 1201542)

grep("I_WAS_NOT_ASCII", iconv(GNI2014$country, "latin1", "ASCII", sub="I_WAS_NOT_ASCII"))
GNI2014$country[c(145, 149)] <- c("Sao Tome and Principe", "Cote d'Ivoire")

save(GNI2014, file = "./pkg/data/GNI2014.rda", compress = "xz")

library(dplyr)

data(GNI2010)

### Fictive data variable: smokers rate per country.
set.seed("20150721")
GNI2010$smokers_rate <- runif(208, min=20, max = 50) + 
    ifelse(GNI2010$continent=="South America", 30, 0) +
    ifelse(GNI2010$continent=="Europe", -20, 0)

# Weighted mean
treemap(GNI2010,
        vSize="population",
        vColor="smokers_rate",
        index=c("continent", "iso3"),
        type="value", 
        fun.aggregate="weighted.mean") %>%
{ .$tm } %>%
    filter( is.na(iso3) ) %>%
    select( continent, vColor, color )

# checking the aggregation results
GNI2010 %>%
    group_by(continent) %>%
    summarise(x=weighted.mean(smokers_rate, w = population))

# Simple mean
treemap(GNI2010,
        vSize="population",
        vColor="smokers_rate",
        index=c("continent", "iso3"),
        type="value", 
        fun.aggregate="mean") %>%
        { .$tm } %>%
    filter( is.na(iso3) ) %>%
    select( continent, vColor, color )

# checking the aggregation results
GNI2010 %>%
    group_by(continent) %>%
    summarise(x=mean(smokers_rate))

# Sum
treemap(GNI2010,
        vSize="population",
        vColor="smokers_rate",
        index=c("continent", "iso3"),
        type="value", 
        fun.aggregate="sum") %>%
        { .$tm } %>%
    filter( is.na(iso3) ) %>%
    select( continent, vColor, color )

# checking the aggregation results
GNI2010 %>%
    group_by(continent) %>%
    summarise(x=sum(smokers_rate))


### test type="dens"
data(business)
treemap(business,
        index=c("NACE1", "NACE2"),
        vSize="turnover",
        vColor="employees",
        type="dens") %>%
        { .$tm } %>%
    filter( is.na(NACE2) ) %>%
    select( NACE1, vColorValue, color )

business %>%
    group_by(NACE1) %>%
    summarise(m=weighted.mean(employees, turnover))



devtools::load_all()
library(dplyr)
library(data.table)
data(business)


df <- data.frame(x=1:2, y=3:4, z=c("foo", "bar"))
treemap(df, index="z", vSize="x", type="index") # all good!

df <- df %>% rowwise %>% mutate(y = x+y) # for this example only
df

class(df)

treemap(df, index="z", vSize="x", type="index")

ungroup(df) %>% treemap(index="z", vSize="x", type="index")


tbl_dt(business) %>%
    treemap(
        index="NACE4",
        vSize="turnover",
        type="index")

tbl_df(business) %>%
    treemap(
        index="NACE4",
        vSize="turnover",
        type="index")


data.table(business) %>%
     treemap(
        index="NACE4",
        vSize="turnover",
        type="index")
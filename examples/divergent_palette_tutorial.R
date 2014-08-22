library(treemap)

## "value" and "comp" types only take divergent color palettes, so RdOrYl is considered as a divergent palette. To see what happens with divergent color palettes, I will use RdYlBu instead:
pal <- "RdYlBu"

## The center color (Yl) will be mapped to 0, negative values to Rd-Yl and positive to Yl-Bu:
dat <- data.frame(letters=letters[1:26], x=1, y=runif(26)*16-8)
treemap(dat, index="letters", vSize="x", vColor="y", type="value", palette=pal)

## Example when there are more extreme negative values than positive:
dat <- data.frame(letters=letters[1:26], x=1, y=runif(26)*16-12)
treemap(dat, index="letters", vSize="x", vColor="y", type="value", palette=pal)

## Note that if there are only positive values, only the Yl-Bu half is used:
dat <- data.frame(letters=letters[1:26], x=1, y=runif(26)*16)
treemap(dat, index="letters", vSize="x", vColor="y", type="value", palette=pal)

## If you want to use a seqential Brewer palette, such as YlOrRd, with type="value", then use a corresponding divergent color palette, i.e. that contains Rd->Yl. Notice that "RdYlBu" also contains Rd->Or and Or->Yl. Use a "-" to reverse the palette so negative values are mapped to the righthand-side of the palette and positive values to the lefthand-side:
dat <- data.frame(letters=letters[1:26], x=1, y=runif(26)*16)
treemap(dat, index="letters", vSize="x", vColor="y", type="value", palette="-RdYlBu")

## Alternatively, use type="manual" to map a specific range of values to the full palette:
treemap(dat, index="letters", vSize="x", vColor="y", type="manual", palette="YlOrRd", range = c(0,16))

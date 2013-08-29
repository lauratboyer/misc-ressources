## add-continents.r
## Defines function add.continents() which adds a quick, low-level
## outline of the world's continents to an existing plot
## (i.e. no need to fuss around with special map classes,
## but the resulting outline is coarse)
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (l.boyer@fisheries.ubc.ca)
## Written on: August 29, 2013
## Time-stamp: <2013-08-29 14:17:45 Laura>

add.continents <- function(...) {

## Extract data for continents
## Datasets rmp and gwm from now defunct package sspline
    if(!exists("rmp")) {
        scd <- getwd()
        setwd("/Users/Laura/Projects/misc-ressources/quick-world-map/")
        rmp <<- read.table("rmp.txt",header=TRUE)
        gwm <<- read.table("gwm.txt",header=TRUE)
        setwd(scd)} # back to original WD

    ind1 <- rmp$inc[-1] # rmp gives the indices for individual polygons in gwm
    gwm$ID <- rep(1:length(ind1), ind1) # assign polygon ID to each lat/lon
    gwm.split <- split(gwm, gwm$ID)

    # only plot polygons that are in the current plot frame
    # as defined by par("usr")

    check.if.in <- function(poly,plus360=0) {

        pu <- par("usr")

        # include polygon is any edge occurs in the current plot
        p.in <- all(any((poly$lon + plus360) %between% pu[1:2]),
              any(poly$lat %between% pu[3:4]))

        # if the polygon is in, return coordinates and add NA to create
        # a cut when the lines() function is used
        # (could also loop through polygons, but this is WAY faster)
        if(p.in) {return(cbind(c(poly$lon+plus360,NA), c(poly$lat,NA)))}
    }

    # test if polygons are in:
    poly.in <- lapply(gwm.split, check.if.in)
    pmat <- do.call(rbind, poly.in)
    # test if polygons are in using shifted longitudes
    # (thank you South Pacific)
    poly.in360 <- lapply(gwm.split, check.if.in, plus360=360)
    pmat360 <- do.call(rbind, poly.in360)

    # ... now add lines to current plot
    lines(pmat, ...)
    lines(pmat360, ...)
}

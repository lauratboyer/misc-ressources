## add-continents.r
## Defines function add.continents() which adds a low-level outline of the world's continents to an existing plot
## -------------------------------------------------------
## Author: Laura Tremblay-Boyer (l.boyer@fisheries.ubc.ca)
## Written on: August 29, 2013
## Time-stamp: <2013-08-29 14:17:45 Laura>

add.continents <- function(lwdd=2) {

## Extract data for continents
## Datasets rmp and gwm from now defunct package sspline
    if(!exists("rmp")) {
        scd <- getwd()
        setwd("/Users/Laura/Projects/misc-ressources/quick-world-map/")
        rmp <<- read.table("rmp.txt",header=TRUE)
        gwm <<- read.table("gwm.txt",header=TRUE)
        setwd(scd)} # back to original WD

    ind1 <- rmp$inc[-1]
    gwm$ID <- rep(1:length(ind1), ind1)
    gwm.split <- split(gwm, gwm$ID)

    # only plot polygon that are in the current plot frame
    check.if.in <- function(poly,plus360=0) {
        pu <- par("usr")

        p.in <- all(any((poly$lon + plus360) %between% pu[1:2]),
              any(poly$lat %between% pu[3:4]))

        if(p.in) {return(cbind(c(poly$lon+plus360,NA), c(poly$lat,NA)))}
    }


    poly.in <- lapply(gwm.split, check.if.in)
    pmat <- do.call(rbind, poly.in)
    poly.in360 <- lapply(gwm.split, check.if.in, plus360=360)
    pmat360 <- do.call(rbind, poly.in360)

    lines(pmat)
    lines(pmat360)

    # old version
    # Prepare data
#    divs=cumsum(rmp)+1; ldiv=1:(nrow(divs)-1)

    # Add lines on both sides of the 360 degree line
#    dmm=sapply(ldiv,
#    function(x) lines(gwm[divs$inc[x]:(divs$inc[x+1]-1),1:2 ], lwd=lwdd))
#    dmm=sapply(ldiv,
#    function(x) lines(360+gwm[divs$inc[x]:(divs$inc[x+1]-1),1],
#                      gwm[divs$inc[x]:(divs$inc[x+1]-1),2], lwd=lwdd))
}

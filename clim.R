source("plotfun.R")
#load("dca.Rdata")  ## takes a bit - 3 GB
load("data/ua.Rdata")

## calculate climatologies


## x must be an array with dimensions for day, month, and year.
## (Might not work if there are no dimensions besides that.)

## takes about 30 seconds for 28k x ydm matrix

climatology <- function(x){
    dnm <- dimnames(x) 
    ndnm <- names(dnm)
    
    stopifnot(all(c("day","month","year") %in% ndnm) &&
              dnm[["day"]] == sprintf("%02i",1:31) &&
              dnm[["month"]] == month.abb)

    marginx <- grep("day|month|year", ndnm, invert=TRUE, value=TRUE)
    clim <- apply(x, marginx, mean, na.rm=TRUE)
    y <- sweep(x, marginx, clim)

    marginy <- grep("day|year", ndnm, invert=TRUE, value=TRUE)    
    seas <- apply(y, marginy, mean, na.rm=TRUE)
    anom <- sweep(y, marginy, seas)
    list(clim=clim, seas=seas, anom=anom)
}


clim <- lapply(ua, climatology)
delta <- list()
delta$clim <- clim$rcp85$clim - clim$hist$clim
delta$seas <- clim$rcp85$seas - clim$hist$seas

save(file="uaclim.Rdata", clim, delta)


######  Plots!

source("plotfun.R")
source("cmaps.R")

#lon=as.numeric(dimnames(ua$hist)$lon)
#lat=as.numeric(dimnames(ua$hist)$lat)




## PAUSE

## actually what I want is:

## DATA

## write to data/clim.Rdata data/delta.Rdata

## climatology: annual & monthly  x ocf
## anomaly: monthly vs ann, daily vs mon  x ocf
## delta: annual & monthly        x oc (bias), cf (change)

## note:
## month clim = ann clim + month anom
## daily anom = daily - month clim = daily - ann clim - month anom


## PLOTS

## annual clim, cols = vars, rows = ocf
## may clim, cols = vars, rows = ocf
## may anom, cols= vars, rows = ocf

## annual delta, cols = vars, rows = bias, change
## may delta, cols = vars, rows = bias, change

## calendar plots (month around outside, annual in middle, see below)
## climatology x ocf x var
## clim (ann) + anom (mon) x ocf x var?



## calendar plot:

# dev.new()
# par(mar=c(1,1,1,1), oma=c(1,1,3,1))
# layout(matrix(nrow=4, ncol=4, byrow=TRUE,
#               c(6, 7, 8, 9,
#                 5,13,13,10,
#                 4,13,13,11,
#                 3, 2, 1,12)))
# v="Q850"
# 
# zra <- srange(clim$hist$seas[v,,,])
# 
# for(m in month.abb){
#     image(lon, lat, clim$hist$seas[v,,,m], zlim=zra,
#           col=anomap[[v]], xaxt='n', yaxt='n', ann=FALSE)
#     title(m)
# }
# 
# image(lon,lat,clim$hist$clim[v,,],
#       col=climap[[v]], xaxt='n', yaxt='n', ann=FALSE)
# title("Annual")



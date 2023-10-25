## Make figure 1 for paper - wet day climatology + diagram of relevant
## processes

library(fields) # for 2x2 obs wet clim plot mixing abs & anom
library(spatstat.geom)

source("plotfun.R")
source("names.R")
source("util.R")
source("~/climod/R/renest.R")

load("data/rdata/misc.Rdata")
load("data/rdata/ua.meta.Rdata")
load("plot/cmaps.Rdata")
load("data/buckets.Rdata")


## TRUE = plot onscreen, FALSE = plot to file
#test = TRUE
test = FALSE

plotdir <- "plot/upper"
system(paste("mkdir -p", plotdir))

## plotting limits
xr <- c(-135,-55)
yr <- c(20,60)
bounds <- list(lon=xr, lat=yr)


infiles <- dir("data/anom", rec=TRUE, full=TRUE)

innames <- strsplit(infiles, '/')  |>
    sapply(tail, 1) |>
    gsub(pat=".Rdata", rep='')

## I should clean this data upstream, but don't have time
clean <- function(x){x[abs(x)>1e20]<-NA; return(x)}

anom <- lapply(infiles, listload) |>   ##listload from util.R
    lapply('[[',1) |>
    setname(innames) |>
    rapply(clean, how="replace") |>
    renest()

################
## Uniform scales for plotting

obs <- grep("obs", names(anom$baseline), val=TRUE)
cur <- grep("hist", names(anom$baseline), val=TRUE)
fut <- grep("rcp85", names(anom$baseline), val=TRUE)

## range types for abs vars
rtype <- c(U850=srange, V850=srange, Q850=zerange, T700=narange, Z700=narange,
           Z500=narange, U250=zerange, V250=srange, A850=zerange, S250=zerange)

absblock <- c(renest(anom$bclim), anom[c("baseline","clim")]) |>
    lapply(abind, along=0) |> abind(along=0)

abslim <- mapply(do.call, as.list(rtype),
                 apply(absblock[,c(obs,cur),,,], 3, list)) |>
    as.data.frame() |> as.list()

## manual override; WRF has large values at SE boundary edge
abslim$A850 <- c(0,85)


loc = "SGP-98-36"
mon = "m05"

testpt <- c(locmap[loc,], list(pch=23, col="black", bg="red"))

mnum <- as.numeric(gsub('m','',mon))
mname <- month.abb[mnum]

if(!test){
    mplotdir <- paste(plotdir, mon, sep='/')
    lplotdir <- paste(mplotdir, loc, sep='/')        
    system(paste("mkdir -p", lplotdir))
}


#############
## obs climatology on wet days

id <-  grep('obs', innames, val=TRUE)

wetclim <- anom$baseline[[id]] + anom$banom[[id]]$wet
wetanom <- anom$delta[[id]]$wet

if(test) {
    dev.new(width=8, height=6)
} else {
    mplotbase <- paste("clim","obs","wet",mon,loc,"png", sep='.')
    png(file=paste0(lplotdir,"/obswetclim.", mplotbase),
        width=8, height=6, units="in", res=120)
}

par(mfrow=c(2,2), mar=c(3,2,3/2,1), oma=c(0,0,2,0))

axargs <- list(mgp=c(3,1,0)/2, cex.axis=0.8)

### moisture advection

imagePlot(lon, lat, wetclim["A850",,], horizontal=TRUE,
          col=climap$A850, zlim=c(0,max(wetclim["A850",,])),
          ann=FALSE, mgp=c(3,1,0)/2, cex.axis=0.8, tcl=-0.4,
          axis.args=axargs, legend.shrink=0.8)

mtext(side=1, adj=0, line=2, cex=0.8, sub(" ","\n",uaunits["A850"]))
title(main="850-mb moisture advection", cex.main=1, line=3/4)
map(add=TRUE, lwd=1/2, "state", ".")
map(add=TRUE, lwd=1/2, "world", c("Can","Mex"))
do.call(points, testpt)

amap <- bwcontrast(climap$A850)[unitize(wetclim["A850",,])*255]
vectorfield(lon, lat, wetclim["U850",,], wetclim["V850",,], col=amap)

### upper circulation

imagePlot(lon, lat, wetclim["S250",,], horizontal=TRUE,
          #          col=tail(climap$S250,-40), zlim=abslim$S250,
          col=climap$S250, zlim=abslim$S250,
          ann=FALSE, mgp=c(3,1,0)/2, cex.axis=0.8, tcl=-0.4,
          axis.args=axargs, legend.shrink=0.8)

mtext(side=1, adj=0, line=2, cex=0.8, sub(" ","\n",uaunits["S250"]))
title(main="250-mb wind speed + direction", cex.main=1, line=3/4)
map(add=TRUE, lwd=1/2, "state", ".")
map(add=TRUE, lwd=1/2, "world", c("Can","Mex"))
do.call(points, testpt)

vectorfield(lon, lat, wetclim["U250",,], wetclim["V250",,], col=1)

### 700-mb temp & pressure

imagePlot(lon, lat, wetanom["T700",,], horizontal=TRUE,
          col=anomap$T700, zlim=srange(wetanom["T700",,]),
          ann=FALSE, mgp=c(3,1,0)/2, cex.axis=0.8, tcl=-0.4,
          axis.args=axargs, legend.shrink=0.8)

mtext(side=1, adj=0, line=2, cex=0.8, sub(" ","\n",uaunits["T700"]))
title(main="700-mb temperature + geopotential anomaly",
      cex.main=1, line=3/4)
map(add=TRUE, lwd=1/2, "state", ".")
map(add=TRUE, lwd=1/2, "world", c("Can","Mex"))
do.call(points, testpt)

clev <- seq(-25, 25, by=5)
clty <- c(rep(3,5), 2, rep(1,5))

contour(lon, lat, wetanom["Z700",,], add=TRUE, method="edge",
        lwd=2, vfont=c("sans serif", "bold"), labcex=0.8,
        col="gray30", levels=clev, lty=clty)

##

mtext(side=3, line=1/2, outer=TRUE, "Observed May wet-day climatology")

##  Diagram!

plot(NA, xlim=range(lon), ylim=range(lat), ann=FALSE, pch=NA)
map(add=TRUE, lwd=1/2, "state", ".") 
map(add=TRUE, lwd=1/2, "world", c("Can","Mex"))
do.call(points, testpt)

#############
## streamline: drop lowest pixel in field.  If it breaks
## connectivity, freeze that pixel as part of shortest path.
## When all pixels dropped, iterate removing branch-ends
## (pixels with only 1 neighbor) until none are left

## slow - don't recalculate if we don't need to
if(!exists("frozen")){

    nx <- length(lon)
    ny <- length(lat)

    working <- wetclim["S250",,]
    frozen <- !is.finite(working)
    frozen[1, which.max(working[1,])] <- TRUE
    frozen[nx, which.max(working[nx,])] <- TRUE

    ## erode working to get frozen path
    for(d in order(working)){
        working[d] <- NA
        cim <- im(is.finite(working) | frozen)
        con <- connected(cim)
        if(length(levels(con$v)) > 1) {
            frozen[d] <- TRUE
        }
    }
    frozen[,1] <- FALSE

    ## remove branches from path
    flag <- TRUE
    while(flag){
        flag <- FALSE
        for(x in 2:(nx-1)){
            for(y in 1:ny){
                if(frozen[x,y]){
                    if(sum(frozen[x+(-1:1), y+(-1:1)]) < 3){                       
                        frozen[x,y] <- FALSE
                        flag <- TRUE
                    }
                }
            }
        }
    }
}

#jcol <- adjustcolor("black",0.3)
jcol <- adjustcolor(tail(climap$S250,1),0.5)

jet <- which(t(frozen), arr=TRUE)

jx <- jet[,"lon"]
jy <- jet[,"lat"]

jlon <- lon[jx]
jlat <- lat[jy]

i4 <- seq(1, nx, by=4)

lines(jlon[i4], jlat[i4], col=jcol, lwd=8)
arrows(jlon[i4],jlat[i4],jlon[i4+4],jlat[i4+4], col="white", lwd=2, length=0.1)
arrows(jlon[i4],jlat[i4],jlon[i4+4],jlat[i4+4], col=jcol, lwd=2, length=0)


#############
## High & low pressure centers

blu <- anomap$T700[43]
red <- anomap$T700[213]
gray <- "gray40"

uadim <- dim(wetanom[1,,])
loind <- arrayInd(which.min(wetanom["Z700",,]), uadim)
hiind <- arrayInd(which.max(wetanom["Z700",,]), uadim)

text(lon[loind[1]], lat[loind[2]], "L", cex=4, col=blu)
text(lon[hiind[1]], lat[hiind[2]], "H", cex=4, col=red)

contour(lon, lat, wetanom["Z700",,], add=TRUE, method="edge",
        lwd=2, drawlabels=FALSE,
#        vfont=c("sans serif", "bold"), labcex=0.6,
        levels=c(-10,0,20), lty=1, col=c(blu, gray, red))


#############
## Advection from the gulf

qflux <- wetclim["A850",,]
qflux[qflux < 55] <- NA
qflux[lon>-80,] <- NA

vectorfield(lon, lat, wetclim["U850",,], wetclim["V850",,],
            lwd=3.5, col=adjustcolor(alpha=0.5,
                                     climap$A850[qflux*256/102]))

title(main="salient features driving precipitation",
      cex.main=1, line=3/4)



##imagePlot(lon, lat, wetclim["A850",,], horizontal=TRUE,
##          col=climap$A850, zlim=c(0,max(wetclim["A850",,])),
##          ann=FALSE, mgp=c(3,1,0)/2, cex.axis=0.8, tcl=-0.4,
##          axis.args=axargs, legend.shrink=0.8)
##
##amap <- bwcontrast(climap$A850)[unitize(wetclim["A850",,])*255]
##

        
if(!test){
    dev.off()
}

## obs May wet-day climatology


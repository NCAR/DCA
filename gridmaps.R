## Make grid plots of climatology, anomaly, etc.

library(abind)

load("data/misc.Rdata")
load("plot/cmaps.Rdata")
source("plotfun.R")
source("names.R")

## plotting limits
#xr <- c(-150,-50)
#yr <- c(15,65)
xr <- c(-135,-55)
yr <- c(20,60)
bounds <- list(lon=xr, lat=yr)

## load data into a list instead of the global environment
listload <- function(filepath){
    load(filepath, temp_env <- new.env())
    as.list(temp_env)
}


## calculate moisture advection
## abind::asub lets us not care how many dimensions there are
calcA <- function(arr){
    u <- asub(arr, "U850", 1)
    v <- asub(arr, "V850", 1)
    q <- asub(arr, "Q850", 1)
    a <- q * sqrt(u^2 + v^2)
    abind(arr, A850=a, along=1, use.dnns=TRUE)
}

total <- listload("data/MPI/anom/total.Rdata") |> rapply(calcA, how="replace")

vars <- c(vars, "A850")
uaunits["A850"] <- paste(uaunits["Q850"], uaunits["U850"])


## looping goes here

xx = "x098"
yy = "y36"
mon = "May"
mm = "m05"

loc = paste0(xx, '.', yy)

modper <- setdiff(periods, "obs")  ## no obs data for buckets

suffix <- paste("",mm,xx,yy,"Rdata", sep=".")
infiles <- paste0("data/",gcm,"/anom/sgp/",loc,"/",modper,suffix)

bdata <- list()
bdata <- lapply(infiles, listload) |> setname(modper)


## crop UA data to CONUS region so that plotted z-ranges are correct

uaconus <- rapply(total, crop, sub=bounds, how="replace")
clon <- lon[lon %within% xr]
clat <- lat[lat %within% yr]

## baselines

ocf <- c("obs","hist","rcp85")  ## plotting order

dev.new(width=13.5, height=4)

gridmap(clon, clat, abind(uaconus$baseline[ocf], along=0), mapcol='black',
        cmaps=climap, units=uaunits,
        main="Baseline upper atmosphere climatology")



## May moisture advection plot

dev.new(width=7, height=11)
#par(mfrow=c(3,1), oma=c(0,0,3,0), mar=c(5,5,5,3)/2, mgp=c(2,2/3,0))

#for(p in ocf){
#    advection(total$totclim[[p]][,,,"May"], main=p)
#}
#mtext("May moisture advection", side=3, outer=TRUE)

gridmap(clon, clat, abind(uaconus$baseline[ocf], along=0)[,"A850",,,drop=FALSE],
        mapcol='black', cmaps=climap, units=uaunits,
        main="May moisture advection")

## bucketized climatology



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

basedata <- abind(uaconus$baseline[ocf], along=0, use.dnns=TRUE) |>
    setname("period", "ndn")

gridmap(clon, clat, basedata, mapcol='black',
        cmaps=climap, units=uaunits,
        main="MPI baseline  upper atmosphere annual climatology")



## May moisture advection plot

maydata <- abind(uaconus$totclim[ocf], along=0, use.dnns=TRUE)[,,,,"May"] |>
    setname("period", "ndn")

facets <- as.data.frame(
    cbind(
        raster  = list("A850",           "Z500",           "T700"),
        vector  = list(c("U850","V850"), c("U250","V250"), NULL),
        contour = list(NULL,             NULL,             "Z700"),
        title   = list(
            qflux  = "850-mb moisture flux",
            hicirc = "250-mb wind + 500-mb geopotential",
            p700   = "700-mb temperature & geopotential")
    )
)


dev.new(width=14, height=9)

gridmap(clon, clat, maydata, facets, cmaps=climap, units=uaunits,
        main="MPI upper atmosphere May climatology", mapcol="black", fatten=TRUE)



## bucketized climatology



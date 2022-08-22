## Make grid plots of climatology, anomaly, etc.

library(abind)

load("data/misc.Rdata")
load("plot/cmaps.Rdata")
source("plotfun.R")
source("names.R")

## plotting limits
xr <- c(-150,-50)
yr <- c(15,65)


## load data into a list instead of the global environment
listload <- function(filepath){
    load(filepath, temp_env <- new.env())
    as.list(temp_env)
}

total <- listload("data/MPI/anom/total.Rdata")


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



## baselines

ocf <- c("obs","hist","rcp85")  ## plotting order

dev.new(width=12, height=4)

gridmap(lon, lat, abind(total$baseline[ocf], along=0), mapcol='black',
        cmaps=climap, units=uaunits, xlim=xr, ylim=yr,
        main="Baseline upper atmosphere climatology")


## bucketized climatology



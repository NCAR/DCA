## Make grid plots of climatology, anomaly, etc.

library(abind)

load("data/misc.Rdata")
load("plot/cmaps.Rdata")
source("plotfun.R")
source("names.R")

xx = "x098"
yy = "y36"
mon = "May"
mm = "m05"

loc = paste0(xx, '.', yy)

modper <- setdiff(periods, "obs")  ## no obs data for buckets

suffix <- paste("",mm,xx,yy,"Rdata", sep=".")
infiles <- paste0("data/",gcm,"/anom/sgp/",loc,"/",modper,suffix)


## load data into a list instead of the global environment

listload <- function(filepath){
    load(filepath, temp_env <- new.env())
    as.list(temp_env)
}


data <- list()
data$bucket <- lapply(infiles, listload) |> setname(modper)
data$total <- listload("data/MPI/anom/total.Rdata")


## baselines

ocf <- c("obs","hist","rcp85")  ## plotting order

dev.new(width=12, height=4)

gridmap(lon, lat, abind(data$total$baseline[ocf], along=0),
        cmaps=climap, units=uaunits, 
        main="Baseline upper atmosphere climatology")


## Total climatology (non-bucketed)
## anom[month] =  totclim[month] - baseline
## 12 months, 3 periods, 8 vars




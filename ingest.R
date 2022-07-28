## ingest and cleanup data

source("names.R")
source("time.R")

library(devtools)
load_all("~/climod")

library(data.table)


#GCM <- "MPI-ESM-LR"
gcm <- "MPI"


#######
## read in precip data

## input files:
## data/gcm/surf/method/period/var/
## var.period.gcm-full.METHOD.day.YYYY-YYYY.NAM-22i.SGP.px.py.nc

## obs files are named similarly, but method=obs, period=obs
## gcm=gridmet for surface, gcm=era for upper atmosphere

## fields in filenames of surface data:
sffields <- c("var", "period", "gcm", "method", "freq",
              "span", "grid", "region", "px", "py", "nc")

## read in all precip files for this GCM
pindirs <- c("data/obs/surf","data/MPI/surf")
#sfiles <- list.files("data/MPI/surf", "prec.*.nc", recursive=TRUE, full=TRUE)
sfiles <- list.files(pindirs, "prec.*.nc", recursive=TRUE, full=TRUE)
ncsurf <- lapply(sfiles, nc_ingest)

## get precip metadata from filenames
pmeta <- gsub(pat=".*/", rep="", sfiles) |>
    strsplit(split='.', fixed=TRUE) |>
    do.call(what=rbind) |>
    setname(sffields, "col") |>
    as.data.frame()


## get dimvar values
periods <- unique(pmeta$period)
perspan <- unique(pmeta[c("period","span")])$span |> setname(periods)
px <- sort(unique(pmeta$px))
py <- sort(unique(pmeta$py))

## reorder methods to organize by type (raw, dyn, mixed, stat)
## note obs is left out to be handled separately
methods <- c("raw","RegCM4","WRF","CNN","LOCA","MBCn","KDDM","SDSM","dummy")


## fold precip timeseries to YMD arrays
plist <- lapply(ncsurf, function(nc){foldtime(nc$prec, nctime2ymd(nc$time))})


## add dummy method = raw data shuffled by month
set.seed(222)

iraw <- which(pmeta$method == "raw")
ssample <- function(x){sample(x) |> setname(names(x))}  ## preserve names
dummy <- lapply(plist[iraw], apply, 2:3, ssample)
plist <- c(plist, dummy)

dmeta <- pmeta[iraw,]
dmeta$method <- "dummy"
pmeta <- rbind(pmeta, dmeta)
row.names(pmeta) <- NULL


## combine folded data slabs into multidim data arrays:
## prec$period[method, px, py, day, month, year]

## Note: pmeta maps info about plist; not needed once multi-dim data
## arrays are created.

## eventually: surf$gcm$var$period[method, x, y, day, month, year]
## or maybe surf is a dataframe with an array element?  (wrap in list)

pdname <- c(list(method=methods, px=px, py=py), dimnames(plist[[1]]))
oname <- pdname; oname$method <- c("obs")

prec <- list()

for(p in periods){
    if(p == "obs"){
        prec[[p]] <- array(NA, dim=sapply(oname, length), dimnames=oname)
    } else {
        prec[[p]] <- array(NA, dim=sapply(pdname, length), dimnames=pdname)
    }
}

for(i in 1:length(plist)){
    b <- pmeta[i,]
    prec[[b$period]][b$method, b$px, b$py,,,] <- plist[[i]]
}


#### now do the same for upper atmosphere (ua) data

## intput files:
## data/gcm/ua/period/
## var.plev.period.gcm-full.raw.day.YYYY-YYYY.CONUS.nc

## fields in filenames of ua data:

uaffields <- c("var", "plev", "period", "gcm", "raw",
               "freq", "span", "region", "nc")

## read in all precip files for this GCM
uindirs <- c("data/MPI/ua", "data/obs/ua")
#uafiles <- list.files("data/MPI/ua", "*.nc", recursive=TRUE, full=TRUE)
uafiles <- list.files(uindirs, "*.nc", recursive=TRUE, full=TRUE)
ncua <- lapply(uafiles, nc_ingest)

## get ua metadata from filenames
uameta <- gsub(pat=".*/", rep="", uafiles) |>
    strsplit(split='.', fixed=TRUE) |>
    do.call(what=rbind) |>
    setname(uaffields, "col") |>
    as.data.frame()

uameta$p <- gsub('p','',uameta$plev)
uameta$pvar <- paste0(uameta$var, uameta$p)


## get dimvar values
periods <- unique(uameta$period)  ## (redundant with surf periods)

## extract coordinate vars
allsame <- function(x){
    y <- lapply(x, c) |> unique()  # c() to drop attributes
    stopifnot(length(y)==1)
    return(y[[1]])
}

lon <- lapply(ncua, `[[`, "lon") |> allsame()
lat <- lapply(ncua, `[[`, "lat") |> allsame()

## drop excessive digits for labels
lats <- sprintf("%5.3f", lat)

uatime <- lapply(ncua, `[[`, "time")

uadimnm <- list(lon=lon, lat=lats, time=NULL)

## extract data vars
uadata <- mapply(`[[`, ncua, uameta$var) |>
    setname(uameta$pvar) |>
    lapply(`dimnames<-`, uadimnm)


## change Q units from kg/kg to g/kg
for(iq in which(uameta$var == "Q")){
    uadata[[iq]] <- uadata[[iq]]*1000
    uadata[[iq]]@units <- "g/kg"
}

## change velocity units from "m s-1" to "m/s"
for(i in 1:length(uadata)){
    uadata[[i]]@units <- gsub("m s-1", "m/s", uadata[[i]]@units)
}

## variable metadata

## vars sorted by standard presentation order
vars <- c("U850","V850","Q850","T700","Z700","Z500","U250","V250")

## units
uaunits <- sapply(uadata, `@`, "units")[vars]

## fold ua data to YMD arrays
## (takes about 5-6 minutes)
ualist <- mapply(foldtimeND, uadata, lapply(uatime, nctime2ymd), SIMPLIFY=FALSE)


## combine folded data slabs into multidim data arrays:
## ua$period[var, lon, lat, day, month, year]

## eventually: ua$gcm$period[var,lon, lat, day, month, year]
## or maybe ua is a dataframe with an array element?  (wrap in list)
## as.data.frame(prec, ua) works; just need to [[]] deref elt to get array

## Note: uameta no longer needed

ua <- list()

for(p in periods){
    ua[[p]] <- abind(var=ualist[uameta$period==p][vars], along=0, use.dnns=TRUE)
    names(dimnames(ua[[p]]))[1] <- "var"
}



##testpt <- "x098.y36"

save(file="data/prec.Rdata", gcm, methods, periods, perspan,
     prec, px, py)

save(file="data/ua.Rdata", gcm, lats, lat, lon, periods, perspan,
     uaunits, ua, vars)

save(file="data/misc.Rdata", gcm, lats, lat, lon, methods, periods,
     perspan, px, py, uaunits, vars)

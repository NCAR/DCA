## ingest and cleanup data

source("names.R")
source("time.R")
source("util.R")

library(devtools)
load_all("~/climod")


#GCM <- "MPI-ESM-LR"
#gcm <- "MPI"

########
### read precip into two data frame, one for obs, one for models
#
### input files:
### data/gcm/surf/method/period/var/
### var.period.gcm-full.METHOD.day.YYYY-YYYY.NAM-22i.SGP.px.py.nc
#
### obs files are named similarly, but method=obs, period=obs
### gcm=gridmet for surface, gcm=era for upper atmosphere
#
### fields in filenames of surface data:
#sffields <- c("var", "period", "gcm", "method", "freq",
#              "span", "grid", "region", "px", "py", "nc")
#
### read in all precip files for this GCM
#pindirs <- c("data/obs/surf","data/MPI/surf")
#sfiles <- list.files(pindirs, "prec.*.nc", recursive=TRUE, full=TRUE)
#ncsurf <- lapply(sfiles, nc_ingest)
#
### get precip metadata from filenames
#pmeta <- gsub(pat=".*/", rep="", sfiles) |>
#    strsplit(split='.', fixed=TRUE) |>
#    do.call(what=rbind) |>
#    setname(sffields, "col") |>
#    as.data.frame()
#
#
### get vectors of indexing values 
#periods <- unique(pmeta$period)
#perspan <- unique(pmeta[c("period","span")])$span |> setname(periods)
#px <- sort(unique(pmeta$px))
#py <- sort(unique(pmeta$py))
#
### reorder methods to organize by type (raw, dyn, mixed, stat)
### note obs is left out to be handled separately
#methods <- c("raw","RegCM4","WRF","CNN","LOCA","MBCn","KDDM","SDSM","dummy")
#
#
### extract precip timeseries
#plist <- lapply(ncsurf, `[[`, "prec")
#
### extract time & convert to YMD
#tlist <- lapply(ncsurf, `[[`, "time") |> lapply(cftime2ymd)
#
#
#
### merge()ing dataframes is clean & idiomatic, but too slow (>10 minutes)
#
### create per-period dataframes by hand
#
#dfs <- list()
#
### create dataframes (per period) with date, px, py, period
#
#for(p in periods){
#    alldates <- unlist(tlist[pmeta$period==p]) |> unique()
#
#    # Not needed for this data
##    checkdates <- ymdreformat(alldates) |>
##        sort() |>
##        ymdreformat("%Y-%m-%d", "%Y-%b-%d")
##    stopifnot(all(alldates == checkdates))
#    
#    dfs[[p]] <- expand.grid(date=alldates, py=py, px=px, period=p,
#                            stringsAsFactors=FALSE)
#}
#
#
### stuff data into dataframe
#
#for(i in 1:nrow(pmeta)){
#    p = pmeta$period[i]
#    pidx <- dfs[[p]]$date %in% tlist[[i]] &
#        dfs[[p]]$px == pmeta$px[i] &
#        dfs[[p]]$py == pmeta$py[i]
#        
#    dfs[[p]][pidx, pmeta$method[i]] <- plist[[i]]
#}
#
#
###  split date to numeric y / m / d
#
#for(p in periods){
#    pymd <- strsplit(dfs[[p]]$date, "-") |> transpose()
#    dfs[[p]]$year <- as.numeric(pymd[[1]])
#    dfs[[p]]$month <- match(pymd[[2]], month.abb)
#    dfs[[p]]$day <- as.numeric(pymd[[3]])    
#}
#
#
#
###  create dummy downscaling method (shuffle data by month)
#modelperiods <- periods[! periods %in% "obs"]
#
#
#set.seed(222)
#
#for(p in modelperiods){
#    d <- split(dfs[[p]], ~ month + year + py + px)
#    e <- lapply(d, function(x){x$dummy <- sample(x$raw);x})
#    dfs[[p]] <- do.call(rbind, e)
#}
#
### probably it would be more efficient not to duplicate the rest of
### the dataframe during split(), but this way I can validate that
### everything matches up afterwards.
#
#
#
### misc cleanup
#
###  convert CNN NA to 0
#for(p in modelperiods){
#    dfs[[p]]$CNN <- clamp(dfs[[p]]$CNN, na=0)
#}
#
#
###  clamp negative precip values to 0
#for(p in modelperiods){
#    for(m in methods){
#        dfs[[p]][[m]] <- clamp(dfs[[p]][[m]], lower=0)
#    }
#}
#
#
###  rearrange columns
#
#pc <- c("date","year","month","day","period","px","py")
#
#prec <- list()
#for(p in modelperiods){
#    prec[[p]] <- dfs[[p]][,c(pc, methods)]
#}
#prec[["obs"]] <- dfs$obs[,c(pc, "obs")]
#
#
### Note: pmeta maps info about plist; not needed once multi-dim data
### arrays are created.
#
### eventually: surf$gcm$var$period[method, x, y, day, month, year]
### or maybe surf is a dataframe with an array element?  (wrap in list)
#

indir <- "~/work/DCA2/data/upper"
outdir <- "~/work/DCA2/data"

## input files:
## indir/gcm/rcm/scen/
## var.plev.scen.gcm-full.rcm."day".YYYY-YYYY."T63.CONUS.nc"

## for raw, rcm = "raw"
## for obs, scen.gcm.rcm = obs.ERAI.gridMET

uafiles <- list.files(indir, "*.nc", recursive=TRUE, full=TRUE)

## fields in filenames of ua data:

uaffields <- c("var", "plev", "scen", "gcm", "rcm",
               "freq", "span", "grid", "region", "nc")

#uindirs <- c("data/MPI/ua", "data/obs/ua")
# uafiles <- list.files(uindirs, "*.nc", recursive=TRUE, full=TRUE)




########

## get ua metadata from filenames
uameta <- gsub(pat=".*/", rep="", uafiles) |>
    strsplit(split='.', fixed=TRUE) |>
    do.call(what=rbind) |>
    setname(uaffields, "col") |>
    as.data.frame()

uameta$p <- gsub('p','',uameta$plev)
uameta$pvar <- paste0(uameta$var, uameta$p)
stop()

## read in data from all files
ncua <- lapply(uafiles, nc_ingest)


## get dimvar values
#periods <- unique(uameta$period)  ## (redundant with surf periods)


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
    uadata[[i]]@units <- gsub("m s-1", "m/s", uadata[[i]]@units) |>
        gsub(pat="m s**-1", rep="m/s", fixed=TRUE, uadata[[i]]@units)
}

## variable metadata

## vars sorted by standard presentation order
vars <- c("U850","V850","Q850","T700","Z700","Z500","U250","V250")

## units
uaunits <- sapply(uadata, `@`, "units")[vars]


## combine data slabs into multidim arrays [var, lon, lat, date]
## plus dataframe(gcm, rcm, scen, span, id]

idcols <- c("gcm","rcm","scen")

fullmeta <- uameta

uameta <- subset(fullmeta, var=="Q", c(idcols,"span"))
uameta$id <- apply(uameta[,idcols], 1, paste, collapse='.')
rownames(uameta) <- NULL

ua <- list()

for(i in 1:nrow(uameta)){

    id <- uameta$id[i]
    cat(id, " ")
    idx <- apply(fullmeta[,idcols], 1, paste, collapse='.') == id
       
    ua[[id]] <- abind(var=uadata[idx][vars], along=0, use.dnns=TRUE)
                                
    names(dimnames(ua[[id]]))[1] <- "var"

    ## extract time coord of first var, convert to date, use as dim

    dimnames(ua[[id]])[[4]] <- cftime2ymd(uatime[[which(idx)[1]]])
    names(dimnames(ua[[id]]))[4] <- "date"
}
cat("\n")


#    ua[[p]] <- abind(var=uadata[uameta$period==p][vars], along=0, use.dnns=TRUE)

#    tindex <- uameta$pvar == vars[1]
#    tcoord <- lapply(uatime[uameta$pvar == vars[1]], cftime2ymd) |>
#        setname(uameta$period[tindex])

## set time coord as dimname on data
#for(p in periods){
#    dimnames(ua[[p]])[[4]] <- tcoord[[p]]
#    names(dimnames(ua[[p]]))[4] <- "date"
#}


save(file=paste0(outdir,"/","ua.Rdata"), ua, uameta, uaunits,
     lats, lat, lon, vars, idcols)

##testpt <- "x098.y36"

#save(file="data/prec.Rdata", gcm, methods, periods, perspan,
#     prec, px, py)

#save(file="data/ua.Rdata", gcm, lats, lat, lon, periods, perspan,
#     uaunits, ua, vars)

#save(file="data/misc.Rdata", gcm, lats, lat, lon, methods, periods,
#     perspan, px, py, uaunits, vars)

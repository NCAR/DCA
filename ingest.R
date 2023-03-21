## ingest and cleanup data

source("names.R")
source("time.R")
source("util.R")

library(devtools)
load_all("~/climod")

indir <- "~/work/DCA2/data/upper"
outdir <- "~/work/DCA2/data/rdata"

## input files:
## indir/gcm/rcm/scen/
## var.plev.scen.gcm-full.rcm."day".YYYY-YYYY."T63.CONUS.nc"

## for raw, rcm = "raw"
## for obs, scen.gcm.rcm = obs.ERAI.gridMET

uafiles <- list.files(indir, "*.nc", recursive=TRUE, full=TRUE)

## fields in filenames of ua data:

uaffields <- c("var", "plev", "scen", "gcm", "rcm",
               "freq", "span", "grid", "region", "nc")

########

## get ua metadata from filenames
uameta <- gsub(pat=".*/", rep="", uafiles) |>
    strsplit(split='.', fixed=TRUE) |>
    do.call(what=rbind) |>
    setname(uaffields, "col") |>
    as.data.frame()

uameta$p <- gsub('p','',uameta$plev)
uameta$pvar <- paste0(uameta$var, uameta$p)

## read in data from all files
ncua <- lapply(uafiles, nc_ingest)


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
#uameta$id <- apply(uameta[,idcols], 1, paste, collapse='.')
#rownames(uameta) <- NULL
rownames(uameta) <- apply(uameta[,idcols], 1, paste, collapse='.')

## R chokes trying to write out the entire list of arrays, so we put
## each one into its own file.  However, we still wrap them each in a
## list with its id as the element name, so that when you read them in
## you can just concatenate them all to reconstruct the list.


#for(i in 1:nrow(uameta)){
for(id in rownames(uameta)){

    ua <- list()
    
    id <- uameta$id[i]
    cat(id, " ")
    idx <- apply(fullmeta[,idcols], 1, paste, collapse='.') == id
       
    ua[[id]] <- abind(var=uadata[idx][vars], along=0, use.dnns=TRUE)
                                
    names(dimnames(ua[[id]]))[1] <- "var"

    ## extract time coord of first var, convert to date, use as dim

    dimnames(ua[[id]])[[4]] <- cftime2ymd(uatime[[which(idx)[1]]])
    names(dimnames(ua[[id]]))[4] <- "date"

    save(file=paste0(outdir,"/ua.",id,".Rdata"), ua)
}
cat("\n")

save(file=paste0(outdir,"ua.meta.Rdata"), uameta, uaunits,
     lats, lat, lon, vars, idcols)


## Calculate climatology and anomalies by method, month, and bucket

load("data/ua.Rdata")
load("data/misc.Rdata")
load("data/buckets.Rdata")
source("names.R")

## We have 3 buckets (dry/moist/wet), plus "all" (no bucket)
## For each bucket, we calculate:
## baseline[1]: average over all time in period
## monthly[12*N]: average over each month
## climatology[12]: average monthly across N years
## monanom[12*N]: monthly minus climatology
## avanom[12]: average monthly anomaly across N years

## And we calculate that for each of 9 methods & 20 locations

## We aren't interested in individual months, so we only keep
## baseline, climatology, and (monthly average) anomaly.  Note also
## that baseline is the same for everything (no per-bucket baseline),
## so we only calculate that once.

## That gives us 2 (clim+anom) * 4 (buckets) * 12 (months) * 9
## (methods) * 20 (locations) * 3 periods) = 51k slices of 8
## variables.  Which is a lot (equivalent to 142 years of data), so
## I'm not going to calculate them all at once or all in one place.
## Calculate per location and month and save in separate files.



## baseline: average of each variable over all time in period

baseline <- lapply(ua, apply, c("var","lat","lon"), mean, na.rm=TRUE)


#for(m in month.abb){
#    for(x in px){
#        for(y in py){

m = "May"
px = "x098"
py = "y36"
p = "hist"

#mua <- ua[[p]][,,,,m,]
## subset to month, wrap YMD to single dimension, fix dimnames
mua <- ua[[p]][,,,,m,,drop=FALSE] |>
    wrap(list(1,2,3,4:6)) |>
    setname(c(dimnamename(ua[[p]])[1:3],"date"),"ndn", 1:4)

#mxybuck <- bprec[[p]][,px,py,,m,]
## subset to loc (dropping dims), then to month & wrap YMD as above
mxybuck <- bprec[[p]][,px,py,,,][,,m,,drop=FALSE] |>
    wrap(list(1,2:4)) |>
    setname(c("method","date"), "ndn", 1:2)

## construct masks for each bucket (+"all" = union of the 3)
bmask <- list()
bmask$all <- mxybuck %in% buckets |> array(dim(mxybuck), dimnames(mxybuck))
for(b in buckets){
    bmask[[b]] <- (mxybuck == b) & bmask$all
}

### convert masks to arrays of index values
#bindex <- lapply(bmask, asplit, MARGIN=1)|>
#    rapply(which, classes="array", how="replace", arr.ind=TRUE)

for(b in c("all", buckets){
    for(mm in methods){

#        mslice <- mua
#        ## average over each month
#        mslice <- mua[,,,iday,iyear] ## that's not what I want...
        mslice <- mua[,,,bmask[[b]][mm,]]
        climatology <- apply(mslice, 1:3, mean, na.rm=TRUE)
        
 #       monthly <- apply(mslice, c("var","lon","lat","year"), mean, na.rm=TRUE)

        
        
    monthly <- ...
    
    ## monthly[12*N]: average over each month
    ## climatology[12]: average monthly across N years
    ## monanom[12*N]: monthly minus climatology
    ## avanom[12]: average monthly anomaly across N years
    
}


#        }
#    }
#}

## monthly averages: 

monthly <- lapply(ua, apply, c("var","lat","lon"), mean, na.rm=TRUE)





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





## Aggregate UA data by bprec[method, px, py, month] and difference from 

## This gives 9x5x4x12 anomalies, but that's equivalent to a bit under
## 6 years worth of daily data, so not bad.




## Then difference then from climatology

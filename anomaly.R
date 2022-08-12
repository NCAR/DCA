## Calculate climatology and anomalies by method, month, and bucket

load("data/ua.Rdata")
load("data/misc.Rdata")
load("data/buckets.Rdata")
source("names.R")

## What gets calculated:

## baseline: the overall average (average over all time in period).
## There's only one baseline; it doesn't vary by bucket or method.

## climatology: per month & bucket (dry/moist/wet) + total (no bucket)
## average across years of all days in that month and bucket.  There's
## only one total climatology, but for the buckets, there's one per
## method and location.

## anomaly: climatology minus baseline

## delta: bucket anomaly minus total anomaly.  

## N = 9 methods * 4 py * 5 px = 180

## Data size:
## baseline[1], clim[12*3*N+12], anomaly = clim, delta=[12*3*N]
## = 19k slices ~= 53 years of data for 8 variables.

## Which is a lot if you don't need it all at once, so I'm going to
## save data in separate files by month and location.



## baseline climatology: average over all time in period
## takes about a minute to run

# system.time(
    baseline <- lapply(ua, apply, 1:3, mean, na.rm=TRUE)
# )

# ## quick check:
# dev.new()
# par(mfrow=c(3,3))
# for(v in vars) image(baseline$hist[v,,], main=v)
# mtext("baseline", outer=TRUE, side=3, line=-1.5)



## var-lon-lat dimensions, used for later steps
cdim <- dim(baseline[[1]])
cdnm <- dimnames(baseline[[1]])

## empty var-lat-lon-mon array for holding results
mdim <- c(cdim, 12)
mdnm <- c(cdnm, list(month=month.abb))
marr <- array(dim=mdim, dimnames=mdnm)

## for indexing
dates <- lapply(ua, dimnames) |> lapply(`[[`, "date")


## monthly total climatology: average over all days in each month and
## across all years in period; takes about a minute

totclim <- list()

#system.time(
for(p in periods){
    totclim[[p]] <- marr
    for(m in month.abb){
        mind <- grep(m, dates[[p]])
        totclim[[p]][,,,m] <- apply(ua[[p]][,,,mind], 1:3, mean, na.rm=TRUE)
    }
}
#)

# ## quick check:
# dev.new()
# par(mfrow=c(3,4))
# for(m in month.abb) image(totclim$obs["T700",,,m], main=m)
# mtext("obs T700 monthly climatology", outer=TRUE, side=3, line=-1.5)



## total monthly anomaly: monthly total climatology minus baseline

totanom <- list()
for(p in periods){
    totanom[[p]] <- sweep(totclim[[p]], 1:3, baseline[[p]])
}

# ## quick check:
# dev.new()
# par(mfrow=c(3,3))
# for(i in 1:8){
#     v=vars[i]; m=month.abb[i]
#     image(totanom$hist[v,,,m], main=paste(v,m))
# }
# mtext("hist total anomaly", outer=TRUE, side=3, line=-1.5)



## save data to file & cleanup

## ! TODO^^^


### Now calculate bucket climatology & anomalies

for(p in periods){
    for(x in px){
        for(y in py){
            ## subset bucket dataframe for p / x / y 
            with(bprec[[p]], xyind <<- px==x & py == y)
            df.pxy <- bprec[[p]][xyind,]            
            for(meth in methods){                
                for(b in buckets){
                    for(mon in month.abb){
# HERE
                    }
#                    mbind <- df.pxy[[meth]] == b
#                    bdays <- df.pxy[mbind, 1:4]

                    ## subset ua array to bucket days for method(p,x,y)
#                    uab <- ua[[p]][,,,mbind]
                    
                    
                
            }
        }
    }
}






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

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


rseq <- function(rng){seq(from=min(rng), to=max(rng))}

## lists of years in each period
peryear <- lapply(perspan,  function(s){
    strsplit(s, '-') |> unlist() |> as.numeric() |> rseq() |> as.character()
})

nyr <- length(peryear[[1]])
stopifnot(all(sapply(peryear, length) == nyr))


## The "all" bucket, which is the same across locations

## baseline climatology: average over all time in period

baseline <- lapply(ua, apply, 1:3, mean, na.rm=TRUE)


# ## quick check:
# dev.new()
# par(mfrow=c(3,3))
# for(v in vars) image(baseline$hist[v,,], main=v)

## var-lon-lat dimensions, used for later steps
cdim <- dim(baseline[[1]])
cdnm <- dimnames(baseline[[1]])


## average over each month
monthly <- list()

for(p in periods){
    cat("-----\n",p,'\n')
    mdim <- c(cdim, nyr, 12)
    mdnm <- c(cdnm, list(year=peryear[[p]], month=month.abb))
    monthly[[p]] <- array(dim=mdim, dimnames=mdnm)

    dd <- dimnames(ua[[p]])$date

    for(y in peryear[[p]]){
        cat(y,'')
        for(m in month.abb){
            cat(m,'')
            ym <- paste0(y, "-", m)
            ymdex <- grep(ym, dd)
            monthly[[p]][,,,y,m] <- apply(ua[[p]][,,,ymdex], 1:3, mean)
        }
        cat("\n")
    }
}

# ## quick check:
# dev.new()
# par(mfrow=c(3,3))
# for(i in 1:8){
#     v=vars[i]; y=peryear$rcp85[i]; m=month.abb[i]
#     image(monthly$rcp85[v,,,y,m], main=paste(v,y,m))
# }


## monthly climatology: average over each month across all years in period

climatology <- lapply(monthly, apply, c(1:3,5), mean)

# ## quick check:
# dev.new()
# par(mfrow=c(3,4))
# for(m in month.abb) image(climatology$obs["T700",,,m], main=m)


## monthly anomaly: monthly average minus (monthly) climatology

monanom <- list()
for(p in periods){
    monanom[[p]] <- sweep(monthly[[p]], c(1:3,5), climatology[[p]])
}


# ## quick check:
# dev.new()
# par(mfrow=c(3,3))
# for(i in 1:8){
#     v=vars[i]; y=peryear$hist[i]; m=month.abb[i]
#     image(monanom$hist[v,,,y,m], main=paste(v,y,m))
# }



## avanom: monthly anomaly averaged across years

avanom <- lapply(monanom, apply, c(1:3,5), mean)

dev.new()
par(mfrow=c(3,4))
for(m in month.abb) image(avanom$rcp85["T700",,,m])


# ## quick check: for all bucket, this is near zero:
# range(unlist(avanom))

rm(avanom)


## save data to file & cleanup


### Now by bucket

for(p in periods){
    for(x in px){
        for(y in py){
            ## subset bucket dataframe for p / x / y 
            with(bprec[[p]], xyind <<- px==x & py == y)
            df.pxy <- bprec[[p]][xyind,]            
            for(meth in methods){                
                for(b in buckets){

                    mbind <- df.pxy[[meth]] == b
                    bdays <- df.pxy[mbind, 1:4]

                    ## subset ua array to bucket days for method(p,x,y)
                    uab <- ua[[p]][,,,mbind]
                    
                    
                    ## baseline: average over all time in period 
                    baseline <- apply(uab, 1:3, mean)

                    ## monthly[12*N]: average over each month

                    ## PAUSE.  For wet bucket, many months have single
                    ## digit days going into them, often zero.  We
                    ## don't want to give a 1-day month equal weight
                    ## with a 12-day month when calculating the
                    ## multi-year average.

                    ## My monthly climatology should be the average of
                    ## all days in this month across multiple years --
                    ## not average of all months.

                    ## And then the anomaly of interest is the
                    ## difference between bucket climatology and all climatology.

                    ## (Which is the same as the difference between
                    ## both of those with the baseline subtracted off,
                    ## since addition is commutative.)


                    ## so what I really want is:

                    ## baseline (average over all time)  <- invariant by bucket, method

                    ## monthly climatology by bucket+all
                    ## (average in month & bucket across years)

                    ## anomaly: climatology - baseline (for each bucket+all)

                    ## delta: bucket anomaly - all anomaly

                    ## and I can stick this all into a function, right?


                    
                    
                    ## climatology[12]: average monthly across N years
                    ## monanom[12*N]: monthly minus climatology
                    ## avanom[12]: average monthly anomaly across N years
                
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

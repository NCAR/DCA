## Calculate climatology and anomalies by method, month, and bucket

print(date())
load("data/ua.Rdata")
load("data/misc.Rdata")
load("data/buckets.Rdata")
source("names.R")
print(date())

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
## save data in separate files by location, month, & period.



## baseline climatology: average over all time in period
## takes about a minute to run

#system.time(
baseline <- lapply(ua, apply, 1:3, mean, na.rm=TRUE)
#)
print(date())

# ## quick check:
# dev.new()
# par(mfrow=c(3,3))
# for(v in vars) image(baseline$hist[v,,], main=v)
# mtext("baseline", outer=TRUE, side=3, line=-1.5)



## empty var-lon-lat array for storing total data
tdim <- dim(baseline[[1]])
tdnm <- dimnames(baseline[[1]])
tarr <- array(dim=tdim, dimnames=tdnm)

## empty var-lat-lon-mon array for holding monthly data
mdim <- c(tdim, 12)
mdnm <- c(tdnm, list(month=month.abb))
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
print(date())

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

outdir <- paste("data", gcm, "anom", sep='/')
system(paste("mkdir -p", outdir))

save(baseline, totclim, totanom, file=paste0(outdir,"/total.Rdata"))
print(date())


## some methods have missing data, which gives NA values in the subset indices
dropna <- function(x){x[!is.na(x)]}

### Now calculate bucket climatology & anomalies

## Takes 2-3 minutes per loop for hist/rcp85 (obs is of course faster)
## Ran start to finish in a little under 2 hours (with no other tasks on the machine.)

for(p in periods){
    cat('--------',p,'\n')
    period = p                                                      ## for save file
    for(x in px){
        for(y in py){
            loc <- paste(x, y, sep='.')                             ## for save file
            cat(loc,'')
            for(mon in month.abb){
                mm <- grep(mon, month.abb) |> sprintf(fmt="m%02d")  ## for save file
                cat(mon,'')
                
                ## subset bucket dataframe for p / x / y / mon
                with(bprec[[p]], sind <<- px==x & py == y & month == mon)
                subdf <- bprec[[p]][sind,]            

                ## storage for calculated results
                climdata <- anomdata <- deltadata <- list()
                
                for(meth in methods){
                    for(b in buckets){
                        mbdates <- subdf$date[subdf[[meth]]==b] |> dropna()
                        subua <- ua[[p]][,,,mbdates[!is.na(mbdates)]]

                        id <- paste(meth, b, sep='.')
                        
                        climdata[[id]]  <- apply(subua, 1:3, mean)
                        anomdata[[id]]  <- climdata[[id]] - baseline[[p]]
                        deltadata[[id]] <- anomdata[[id]] - totanom[[p]][,,,m]
                    }
                }
                
                ## create output dataframes with array column for bucket values
                clim <- anom <- delta <- expand.grid(method=methods, bucket=buckets)
                clim$ua  <- climdata
                anom$ua  <- anomdata
                delta$ua <- deltadata
                
                ## save data
                savedir <- paste(outdir, "sgp", loc, sep='/')
                system(paste("mkdir -p", savedir))
                filename <- paste(p, mm, loc, "Rdata", sep='.')
                
                save(gcm, loc, mon, period, clim, anom, delta,
                     file=paste0(savedir, '/', filename))
            }
            cat("\n")
            print(date())
        }
    }
}

print(date())

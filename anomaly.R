## Calculate climatology and anomalies by method, month, and bucket

source("names.R")
load("data/buckets.Rdata")
load("data/prec.SGP.all.Rdata")

## What gets calculated:

## baseline: the overall average (average over all time in period).
## There's only one baseline; it doesn't vary by bucket or method.

## climatology: per month & bucket (dry/moist/wet) + total (no bucket)
## average across years of all days in that month and bucket.  There's
## only one total climatology, but for the buckets, there's one per
## method and location.

## anomaly: climatology minus baseline

## delta: bucket anomaly minus total anomaly.  

## Data size:

## 3 GCMs * 3 dynamical methods * 2 scenarios + ERAI = 18 runs + 1

## 1 baseline per run
## 12 months * 1 baseline = 12 total climatologies per run

## 20 locs * 3 buckets * 3 gcms * 2 scenarios * 9 methods * 12 months
## = 38880 bucket anomalies

## 1 delta per bucket anomaly

## ~= 100 years of data for 8 variables.

## Which is a lot if you don't need it all at once, so we loop over
## run, month, & location, and store things separately.  Files are
## organized month/location/gcm/scen, and then within each directory
## we have one file for each method.

outroot <- "data/anom"
#system(paste("mkdir -p", outroot))

indir <- "data/rdata"
infiles <- dir(indir, pattern="ua\\..*\\..*\\..*\\.Rdata")

## non-statistical downscaling methods
nonstat <- strsplit(infiles, '.', fixed=TRUE) |> sapply('[',3) |> unique()
## statistical downscaling methods
statmeth <- (function(x,y){x[!x %in% y]})(levels(prec$method), nonstat)

mnum <- paste0("m", sprintf("%02d",1:12)) |> setname(month.abb)

for(i in 1:length(infiles)){

    print(infiles[i])
    load(paste0(indir,"/",infiles[i]))    # ~3 sec/file

    ## unwrap list - don't use unlist(); it flattens to vector
    id <- names(ua)
    ua <- ua[[1]]

    bits <- strsplit(id, '.', fixed=TRUE) |> unlist()
    GCM <- bits[1] |> gsub(pat='-.*', rep='')  ## allcaps for subset(prec)
    rcm <- bits[2]
    scen <- bits[3]

    ## baseline climatology: average over all time in period
    ## ~5 sec /file
    cat("baseline ")
    baseline <- apply(ua, 1:3, mean, na.rm=TRUE)


    # ## quick check:
    # dev.new()
    # par(mfrow=c(3,3))
    # vars <- dimnames(ua)[[1]]
    # for(v in vars) image(baseline[v,,],main=v)
    # mtext("baseline", outer=TRUE, side=3, line=-1.5)

    ## indexing
    dates <- dimnames(ua)$date
    midx <- lapply(month.abb, grep, dates) |> setname(month.abb)

    ## monthly total climatology: average over all days in each month and
    ## across all years; ~5 sec/file

    cat("clim ")
    
    totclim <- list()

    for(m in month.abb){
        totclim[[m]] <- apply(ua[,,,midx[[m]]], 1:3, mean, na.rm=TRUE)
    }


    # ## quick check:
    # dev.new()
    # par(mfrow=c(3,4))
    # for(m in month.abb) image(totclim[[m]]["T700",,], main=m)
    # mtext("T700 monthly climatology", outer=TRUE, side=3, line=-1.5)


    ## total monthly anomaly: monthly total climatology minus baseline

    cat(" anom")
    totanom <- lapply(totclim, sweep, 1:3, baseline)

    ## quick check:
    # dev.new()
    # par(mfrow=c(12,8), mar=c(1,1,1,1)/2)
    # for(m in month.abb){
    #     for(v in vars){
    #         image(totanom[[m]][v,,], xaxt='n', yaxt='n')
    #     }
    # }


    dmethods <- if(rcm == "raw"){ c("raw", statmeth) } else { rcm }

    for(mon in "May"){
#    for(mon in month.abb){
        cat(mon, " ")
        for(loc in "SGP-98-36"){
#        for(loc in unique(prec$locname)){
            cat(loc, " ")

            outdir <- paste(outroot, mnum[mon], loc, GCM, scen, sep='/')
            system(paste("mkdir -p", outdir))

            subprec <- subset(prec, locname==loc & scen==bits[3] &
                                    month.abb[month]==mon & gcm==GCM)

            mua <- ua[,,, grep(mon, dates)]

            for(meth in dmethods){
                cat(meth, " ")

                ## TODO: if CNN & not MPI, need to drop leap days
                
                mprec <- subset(subprec, method==meth)
                bprec <- bucketize(mprec$prec)
                
                ## bucket climatology

                bclim <- list()
                for(b in buckets){
                    bclim[[b]] <- apply(mua[,,,bprec==b], 1:3,
                                        mean, na.rm=TRUE)
                }
                
                ## bucket anomaly (bclim - baseline)
                banom <- lapply(bclim, sweep, 1:3, baseline)
                
                ## delta (bucket anomaly - total anomaly)
                delta <- lapply(banom, '-', totanom[[mon]])

                #    ## quick check
                #    dev.new()
                #    par(mfcol=c(3,4))
                #    v = "T700"
                #    image(baseline[v,,], main="base")
                #    image(totclim[[mon]][v,,], main="clim")
                #    image(totanom[[mon]][v,,], main="amon")
                #    for(b in buckets){
                #        image(bclim[[b]][v,,], main=paste(b, "clim"))
                #        image(banom[[b]][v,,], main=paste(b, "anom"))
                #        image(delta[[b]][v,,], main=paste(b, "delta"))
                #    }

                ## metadata (method, scen, GCM, loc, month) goes only
                ## in filename so that contents of upper[[]] are
                ## uniformly anomaly arrays, which is how we want to
                ## interact with them in gridmaps.R
                                
                ## save to file
                outfile <- paste(meth,scen,GCM,loc,mnum[mon],"Rdata", sep='.')

                upper <- list(baseline=baseline, clim=totclim[[mon]],
                              anom=totanom[[mon]], bclim=bclim,
                              banom=banom, delta=delta)

                save(upper, file=paste0(outdir,"/",outfile))
            }  # method
        } # loc
        cat("\n")
    } # month
    cat("\n")
} # infile


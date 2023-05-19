## Make grid plots of climatology, anomaly, etc.

library(abind)

source("plotfun.R")
source("names.R")
source("util.R")
source("~/climod/R/renest.R")

load("data/rdata/misc.Rdata")
load("data/rdata/ua.meta.Rdata")
load("plot/cmaps.Rdata")
load("data/buckets.Rdata")


## TRUE = plot onscreen, FALSE = plot to file
#test = TRUE
test = FALSE

plotdir <- "plot/upper"
system(paste("mkdir -p", plotdir))

## plotting limits
xr <- c(-135,-55)
yr <- c(20,60)
bounds <- list(lon=xr, lat=yr)


infiles <- dir("data/anom", rec=TRUE, full=TRUE)

innames <- strsplit(infiles, '/')  |>
    sapply(tail, 1) |>
    gsub(pat=".Rdata", rep='')

ameta <- strsplit(innames, '.', fixed=TRUE) |>
    do.call(what=rbind) |>
    data.frame() |>
    setname(c("method","scen","gcm","loc","month")) |>
    setname(innames, "row")

## factors ordered for sorting
ameta$scen <- factor(ameta$scen, levels=scen)
ameta$gcm   <- gsub("2","", ameta$gcm) |> factor(levels=gcms)
ameta$method <- factor(ameta$method, levels=methods)

ameta <- ameta[with(ameta, order(scen, gcm, method)),]

## I should clean this data upstream, but don't have time
clean <- function(x){x[abs(x)>1e20]<-NA; return(x)}

anom <- lapply(infiles, listload) |>   ##listload from util.R
    lapply('[[',1) |>
    setname(innames) |>
    rapply(clean, how="replace") |>
    renest()


################
## Uniform scales for plotting

obs <- grep("obs", names(anom$baseline), val=TRUE)
cur <- grep("hist", names(anom$baseline), val=TRUE)
fut <- grep("rcp85", names(anom$baseline), val=TRUE)

## range types for abs vars
rtype <- c(U850=srange, V850=srange, Q850=zerange, T700=narange, Z700=narange,
           Z500=narange, U250=zerange, V250=srange, A850=zerange, S250=zerange)

absblock <- c(renest(anom$bclim), anom[c("baseline","clim")]) |>
    lapply(abind, along=0) |> abind(along=0)

abslim <- mapply(do.call, as.list(rtype),
                 apply(absblock[,c(obs,cur),,,], 3, list)) |>
    as.data.frame() |> as.list()

## manual override; WRF has large values at SE boundary edge
abslim$A850 <- c(0,85)


chgblock <- absblock[,fut,,,] - absblock[,cur,,,]
changelim <- apply(chgblock, 3, srange) |>
    as.data.frame() |> as.list()


anomblock <- abind(anom$anom, along=0)
anomlim <- apply(anomblock[c(obs, cur),,,], 2, srange) |>
    as.data.frame() |> as.list()

## manual override: zg gets contoured, is mostly positive
anomlim$Z700 <- narange(anomblock[c(obs,cur),"Z700",,])


deltablock <- lapply(anom$delta, abind, along=0) |> abind(along=0)

deltalim <- apply(deltablock[c(obs,cur),,,,], 3, srange) |>
    as.data.frame() |> as.list()


#################
## analysis loops

for(loc in unique(ameta$loc)){
    testpt <- c(locmap[loc,], list(pch=23, col="black", bg="red"))

    for(mon in unique(ameta$month)){
        mnum <- as.numeric(gsub('m','',mon))
        mname <- month.abb[mnum]

        for(meth in dynmethods) {

            if(!test){
                mplotdir <- paste(plotdir, mon, sep='/')
                lplotdir <- paste(mplotdir, loc, sep='/')        
                system(paste("mkdir -p", lplotdir))
                mplotbase <- paste(meth, mon, "png", sep='.')
                lplotbase <- paste(meth, mon, loc, "png", sep='.')
            }

            
            ## which anomalies to plot for baseline & clim
            baseids <- rownames(ameta)[with(ameta, month==mon &
                                                   ( scen=="obs" |
                                                     (scen=="hist" &
                                                      method==meth)))]
            
            gnames <- strsplit(baseids, '.', fixed=TRUE) |> sapply('[',3)
            gnamelist <- list(gcm=gnames, var=NULL, lon=NULL, lat=NULL)

            
            ################
            ## baseline climate
            
            ## bind data to be plotted into array[ens,var,lon,lat]
            basedata <- abind(anom$baseline[baseids], along=0,
                              use.dnns=TRUE, new.names=gnamelist)

            ################
            ## baseline plot

            if(test) {
                dev.new(width=15, height=5.5)
            } else {
                png(file=paste0(plotdir,"/baseline.", meth, ".png"),
                    width=15, height=5.5, units="in", res=120)
            }
            
            main <- paste("hist", meth,
                          "baseline upper atmosphere climatology")
            gridmap(lon, lat, basedata, mapcol='black', zlims=abslim,
                    cmaps=climap, units=uaunits, main=main)
            
            if(!test){ dev.off() }

            
            ##########
            ## monthly climatology
            
            mondata <- abind(anom$clim[baseids], along=0,
                             use.dnns=TRUE, new.names=gnamelist)

            
            ###########
            ## combined monthly climatology plot

            ## 
            
            facets <- as.data.frame(
                cbind(
                 raster  = list("A850",           "T700", "S250"),
                 vector  = list(c("U850","V850"), NULL,   c("U250","V250")),
                 contour = list(NULL,             "Z700", NULL),
                 title   = list(
                        qflux  = "850-mb moisture advection",
                        p700   = "700-mb temperature + geopotential",
                        hicirc = "250-mb wind speed + direction")
                )
            )

            if(test) {
                dev.new(width=10, height=9)
            } else {            
                png(file=paste0(mplotdir,"/monclim.", mplotbase),
                    width=10, height=9, units="in", res=120)
            }

            main <- paste("hist", meth, mname,
                          "upper atmosphere climatology")
            gridmap(lon, lat, mondata, facets, cmaps=climap, zlims=abslim,
                    units=uaunits, main=main, mapcol="black")

            if(!test) {dev.off()}
            
            ###########
            ## monthly climatology change

            futids <- rownames(ameta)[with(ameta, month==mon &
                                                  scen=="rcp85" &
                                                  method==meth)]

            fnames <- strsplit(futids, '.', fixed=TRUE) |> sapply('[',3)
            fnamelist <- list(gcm=fnames, var=NULL, lon=NULL, lat=NULL)

            ### future baseline unused
            # futdata <- abind(anom$baseline[futids], along=0,
            #                 use.dnns=TRUE, new.names=fnamelist)

            futmondata <- abind(anom$clim[futids], along=0,
                             use.dnns=TRUE, new.names=fnamelist)

            changedata <- futmondata[gcms[-1],,,] - mondata[gcms[-1],,,]
            
            
            ###########
            ## monthly climatology change plot

            if(test) {
                dev.new(width=10, height=7)
            } else {            
                png(file=paste0(mplotdir,"/change.", mplotbase),
                    width=10, height=7, units="in", res=120)
            }            
            
            main <- paste(meth, mname, "climatology change,",
                          uameta$span[3],"vs", uameta$span[2])
            gridmap(lon, lat, changedata, facets, cmaps=anomap,
                    zlims=changelim, units=uaunits, main=main,
                    mapcol="black", arrowcol="darkgray")
            
            if(!test){ dev.off() }

            

            ##########
            ## monthly anomaly vs baseline climatology
            
            anomdata <- abind(anom$anom[baseids], along=0,
                              use.dnns=TRUE, new.names=gnamelist)

            ##########
            ## monthly anomaly plot

            if(test) {
                dev.new(width=10, height=9)
            } else {            
                png(file=paste0(mplotdir,"/anomaly.", mplotbase),
                    width=10, height=9, units="in", res=120)
            }
            
            main <- paste("hist", meth, mname,
                          "upper atmosphere anomaly vs climatology")
            gridmap(lon, lat, anomdata, facets, cmaps=anomap, zlims=anomlim,
                    units=uaunits, main=main, mapcol="black",
                    arrowcol="darkgray")
            
            if(!test){ dev.off() }


            #############
            ## bucketized climatology

            for(scenario in scen[-1]){  ## not obs
                
                ## short titles
                bfacets <- facets
                bfacets$title <- c("850-mb Qflux",
                                   "700-mb T & Z",
                                   "250-mb winds")

                ## names of anomalies to plot
                baseids <- rownames(ameta)[with(ameta, month==mon &
                                                       ( scen=="obs" |
                                                         (scen==scenario &
                                                          method==meth)))]
                
                
                ## all methods by bucket

                if(meth == "raw") {
                    bmethods <- c("raw", statmethods)
                } else {
                    bmethods <- meth
                }

                for(bmeth in bmethods){
                    if(!test){
                        bpbase <- sub("raw", bmeth, lplotbase)
                    }
                    bbaseids <- sub("raw", bmeth, baseids)
                    deltadata <- lapply(renest(anom$delta[bbaseids]), abind,
                                        along=0, use.dnns=TRUE,
                                        new.names=gnamelist)

                    bfreq <- subset(bstat, month == mnum & locname == loc &
                                           method %in% c("gridMET", bmeth) &
                                           scen %in% c("obs", scenario))
                    
                    
                    ## delta (bucket anom - month anom)
                    for(b in buckets){
                        
                        if(test) {
                            dev.new(width=10, height=9)
                        } else {
                            png(file=paste0(lplotdir,"/delta.", scenario, ".", b, ".", bpbase),
                                width=10, height=9, units='in', res=120)
                        }
                        
                        main <- paste(bmeth, mname, "UA", b,
                                      "anomaly difference,", scenario, loc)

                        gridmap(lon, lat, deltadata[[b]], bfacets, cmaps=anomap,
                                zlims=deltalim, units=uaunits, main=main,
                                mapcol="black", pointargs=testpt,
                                arrowcol="darkgray", concol="darkgray",
                                margi=c(2,5,5,3,2,2)/8, concex=0.6)

                        pct <- with(subset(bfreq, bucket==b),
                                    pct[match(gcm, gnames)]) |>
                            sprintf(fmt="%#.1f%% of days")
                        par(mfrow=c(1,1), oma=c(0,2,0,0))
                        mtext(pct, side=2, outer=TRUE, line=1/2, cex=0.8,
                              at=(4:1*2-1)/9+0.07)

                        if(!test){
                            dev.off()
                        }
                    } ## buckets
            } ## bmethods
            } ## scenario
        } ## meth
    }  ## mon
}  ## loc


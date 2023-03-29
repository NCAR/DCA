## Make grid plots of climatology, anomaly, etc.

library(abind)

source("plotfun.R")
source("names.R")
source("~/climod/R/renest.R")


## load data into a list instead of the global environment
listload <- function(filepath){
    load(filepath, temp_env <- new.env())
    as.list(temp_env)
}

load("data/rdata/misc.Rdata")
load("data/rdata/ua.meta.Rdata")
load("plot/cmaps.Rdata")

plotdir <- "plot/upper"
system(paste("mkdir -p", plotdir))

## plotting limits
xr <- c(-135,-55)
yr <- c(20,60)
bounds <- list(lon=xr, lat=yr)


## calculate moisture advection
## abind::asub lets us not care how many dimensions there are
calcA <- function(arr, Dim=1){
    u <- asub(arr, "U850", Dim)
    v <- asub(arr, "V850", Dim)
    q <- asub(arr, "Q850", Dim)
    a <- q * sqrt(u^2 + v^2)
    abind(arr, A850=a, along=Dim, use.dnns=TRUE)
}

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
ameta$scen <- factor(ameta$scen, levels=scen)#misc$scen)
ameta$gcm   <- gsub("2","", ameta$gcm) |> factor(levels=gcms)#misc$gcms)
ameta$method <- factor(ameta$method, levels=methods)#misc$method)

ameta <- ameta[with(ameta, order(scen, gcm, method)),]

## I should clean this data upstream, but don't have time
clean <- function(x){x[abs(x)>1e20]<-NA; return(x)}

anom <- lapply(infiles, listload) |>
    lapply('[[',1) |>
    setname(innames) |>
    rapply(clean, how="replace") |>
    rapply(calcA, how="replace") |>
    renest()

vars <- c(vars, "A850")
uaunits["A850"] <- paste(uaunits["Q850"], uaunits["U850"])



## looping goes here

for(loc in unique(ameta$loc)){
    for(mon in unique(ameta$month)){

        mname <- month.abb[as.numeric(gsub('m','',mon))]
#
#x = -98
#y = 36
#xx = "x098"
#yy = "y36"
#mon = "May"
#mm = "m05"
#
#loc = paste0(xx, '.', yy)

#mplotdir <- paste(plotdir, mm, sep='/')
#system(paste("mkdir -p", mplotdir))


# modper <- setdiff(periods, "obs")  ## no obs data for buckets
# 
# suffix <- paste("",mm,xx,yy,"Rdata", sep=".")
# infiles <- paste0("data/",gcm,"/anom/sgp/",loc,"/",modper,suffix)
# 
# bdata <- list()
# bdata <- lapply(infiles, listload) |> setname(modper)


# ## crop UA data to CONUS region so that plotted z-ranges are correct
# 
# uaconus <- rapply(total, crop, sub=bounds, how="replace")
# clon <- lon[lon %within% xr]
# clat <- lat[lat %within% yr]

## baselines

        ## overall baseline: obs / hist GCMS x qflux, tz 700, highcirc
        ## may climatology: ditto
        ## may anomaly: by GCM, scen, delta colorbars
        ## bucket anomaly: by GCM, scen; method x vars
        ## delta: by GCM, scen; method x vars

        
#ocf <- c("obs","hist","rcp85")  ## plotting order

#dev.new(width=13.5, height=4)
#png(file=paste0(mplotdir,"/baseline.",mm,".png"),
#    width=13.5, height=4, units="in", res=120)

#basedata <- abind(uaconus$baseline[ocf], along=0, use.dnns=TRUE) |>
#    setname("period", "ndn")

        ## which anomalies to plot for baseline & clim
        baseids <- rownames(ameta)[with(ameta, month==mon &
                                               ( scen=="obs" |
                                                 (scen=="hist" &
                                                  method=="raw")))]
        #                          method %in% dynmethods)))]
        
        gnames <- strsplit(baseids, '.', fixed=TRUE) |> sapply('[',3)
        gnamelist <- list(gcm=gnames, var=NULL, lon=NULL, lat=NULL)
        
        ## bind data to be plotted into array[ens,var,lon,lat]
        basedata <- abind(anom$baseline[baseids], along=0,
                          use.dnns=TRUE, new.names=gnamelist)

        ## range types
        rtype <- c(U850=srange, V850=srange, Q850=zerange,
                   T700=narange, Z700=narange, Z500=narange,
                   U250=narange, V250=srange, A850=zerange)

        ## ranges
        blim <- list()
        for(v in vars){
            blim[[v]] <- rtype[[v]](basedata[,v,,])
        }

        main <- paste("Baseline (annual) upper atmosphere climatology,",
                      "hist", loc)
        gridmap(lon, lat, basedata, mapcol='black', zlims=blim,
                cmaps=climap, units=uaunits, main=main)


#dev.copy2pdf(file=paste0(mplotdir,"/baseline.",mm,".pdf"),
#             width=13.5, height=4, title=main)
#dev.off()


## combined monthly climatology plot

#maydata <- abind(uaconus$totclim[ocf], along=0, use.dnns=TRUE)[,,,,"May"] |>
#    setname("period", "ndn")

        mondata <- abind(anom$clim[baseids], along=0,
                         use.dnns=TRUE, new.names=gnamelist)

        mlim <- list()
        for(v in vars){
            mlim[[v]] <- rtype[[v]](mondata[,v,,])
        }
        
        
        facets <- as.data.frame(
            cbind(
                raster  = list("A850",           "T700", "Z500"),
                vector  = list(c("U850","V850"), NULL,   c("U250","V250")),
                contour = list(NULL,             "Z700", NULL),
                title   = list(
                    qflux  = "850-mb moisture advection",
                    p700   = "700-mb temperature + geopotential",
                    hicirc = "250-mb wind + 500-mb geopotential")
            )
        )


dev.new(width=12, height=12)
#png(file=paste0(mplotdir,"/advection.",mm,".png"), 
#    width=14, height=9, units="in", res=120)

        main <- paste(mname, "upper atmosphere climatology,",
                      "hist", loc)
        gridmap(lon, lat, mondata, facets, cmaps=climap, zlims=mlim,
                units=uaunits, main=main, mapcol="black")

#dev.copy2pdf(file=paste0(mplotdir,"/advection.",mm,".pdf"),
#             width=14, height=9, title=main)
# dev.off()

        ## monthly anomaly vs baseline climatology

        anomdata <- abind(anom$anom[baseids], along=0,
                           use.dnns=TRUE, new.names=gnamelist)

        alim <- list()
        for(v in vars){
            alim[[v]] <- srange(anomdata[,v,,])
        }

        main <- paste(mname, "upper atmosphere anomaly vs climatology,",
                      "hist", loc)
        gridmap(lon, lat, anomdata, facets, cmaps=anomap, zlims=alim,
                units=uaunits, main=main, mapcol="black")
        
        

## bucketized climatology

## 
## clim = by month & bucket  (clim/total = maydata above)
## anom = clim minus baseline
## delta = bucket anomaly - total anomaly

    
#bplotdir <- paste(mplotdir, loc, sep='/')
#system(paste("mkdir -p", bplotdir))

#
#
### crop UA data to CONUS & calculate q-flux
#bconus <- rapply(bdata, crop, sub=bounds, how="replace", classes="array") |>
#    rapply(calcA, how="replace", classes="array", Dim=3)
#
## symmetric z-ranges around zero
slim <- apply(bconus$hist$delta, 3, srange, simplify=FALSE)


## short titles
bfacets <- facets
bfacets$title <- c("850-mb Qflux", "700-mb T & Z", "high circul'n")


## all methods by bucket

for(b in buckets){
#    dev.new(width=7, height=12)
    png(file=paste0(bplotdir,"/anom.bucket.",mm,".",b,".png"),
        width=7, height=12, units='in', res=120)

    main <- paste(gcm, "UA", mon, b, "anomaly difference")
    ambb <- bconus$hist$delta[b,,,,]

    gridmap(clon, clat, ambb, bfacets, cmaps=anomap, zlims=slim,
            units=uaunits, mapcol='black', main=main, alen=0,
            arrowcol='darkgray', concol="darkgray", concex=0.6)

#    dev.copy2pdf(file=paste0(bplotdir,"/anom.bucket.",mm,".",b,".pdf"),
#             width=7, height=12, title=main)
    dev.off()
}



## all buckets by method

testpt <- list(x=x, y=y, pch=23, col="black", bg="red")

for(m in methods){
#    dev.new(width=14, height=9)
    png(file=paste0(bplotdir,"/anom.method.",mm,".",m,".png"),
        width=14, height=9, units='in', res=120)
    main <- paste(gcm,"+",m, "UA", mon, "anomaly difference")
    abbm <- bconus$hist$delta[,m,,,]

    gridmap(clon, clat, abbm, facets, cmaps=anomap, zlims=slim,
            units=uaunits, mapcol='black', main=main, pointargs=testpt,
            arrowcol='dimgray', concol="dimgray", concex=0.8)

#    dev.copy2pdf(file=paste0(bplotdir,"/anom.method.",mm,".",m,".pdf"),
#                 width=14, height=9, title=main)
    dev.off()
}



## faceted approach using ggplot

library(ggplot2)
library(reshape2)  
library(maps)
library(plyr)
library(gridExtra)

defactor <- function(x) {as.numeric(levels(x))[x]}

## ggplot wants data in long format
## it's 6x bigger, but not a problem at the monthly scale

totclim <- abind(uaconus$totclim[ocf], along=0, use.dnns=TRUE) |>
     setname("period", "ndn") |>
    adply(c(1,3,4,5))

## we do need to convert lat & lon back to numeric
totclim$lon <- defactor(totclim$lon)
totclim$lat <- defactor(totclim$lat)

rawmay <- subset(totclim, month=="May")


## wind polygons

w850scale <- min(diff(clon),diff(clat))/max(mlim$U850, mlim$V850)
w250scale <- min(diff(clon),diff(clat))/max(mlim$U250, mlim$V250)

w850 <- with(rawmay, polyfield(U850, V850, lon, lat, scale=w850scale, pad=FALSE))
w250 <- with(rawmay, polyfield(U250, V250, lon, lat, scale=w250scale, pad=FALSE))

foo <- mapply(cbind, id=1:length(w850), w850, SIMPLIFY=FALSE) 
bar <- do.call(rbind, foo)
baz <- data.frame(id=1:nrow(rawmay), value=rawmay$A850)
buz <- merge(bar, baz, by=c("id"))

ggplot(buz, aes(x=x, y=y, asp=1)) + 
    geom_polygon(aes(fill=value, group=id))+
    borders("state", xlim=xr, ylim=yr, lwd=1/8, col="black")


base <- ggplot(rawmay, aes(x=lon, y=lat)) +
    xlim(xr) + ylim(yr) +
    coord_fixed() + ## square aspect ratio 
    theme(legend.position="bottom",
          axis.title.x = element_blank(),
          axis.title.y = element_blank()) +
    borders("state", xlim=xr, ylim=yr, lwd=1/8, col="black")+
    borders("world",c("Can","Mex"), xlim=xr, ylim=yr, lwd=1/8, col="black")

qflux <- base + 
    geom_tile(aes(fill=A850)) + ## raster plot
    scale_fill_gradientn(colors=climap[["A850"]],
                         limits=blim[["A850"]]) +
    facet_grid(vars(period)) +
    geom_segment(aes(xend=lon+U850*w850scale, yend=lat+V850*w850scale))


hicirc <- base + 
    geom_tile(aes(fill=Z500)) + ## raster plot
    scale_fill_gradientn(colors=anomap[["Z500"]],
                         limits=blim[["Z500"]])+
    facet_grid(vars(period)) +
    geom_segment(aes(xend=lon+U250*w250scale, yend=lat+V250*w250scale))

grid.arrange(qflux, hicirc, nrow=1)


## Okay, that's pretty slick.

## Have to figure out how to rearrange layers, but I think it's good.

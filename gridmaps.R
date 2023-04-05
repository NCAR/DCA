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
test = FALSE

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

## calculate wind speed
calcS <- function(arr, Dim=1){
    u <- asub(arr, "U250", Dim)
    v <- asub(arr, "V250", Dim)
    s <- sqrt(u^2 + v^2)
    abind(arr, S250=s, along=Dim, use.dnns=TRUE)
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
    rapply(calcA, how="replace") |>
    rapply(calcS, how="replace") |>
    renest()

vars <- c(vars, "A850", "S250")
uaunits["A850"] <- paste(uaunits["Q850"], uaunits["U850"])
uaunits["S250"] <- uaunits["U250"]


### HACK!  Temporary fix to WRF zg, which is off by x10
for(i in grep("WRF", innames)){
    for(a in 1:6){
        for(z in c("Z700","Z500")){
            if(a <= 3){
                anom[[a]][[i]][z,,] <- anom[[a]][[i]][z,,] / 10
            } else {
                for(b in buckets){
                    anom[[a]][[i]][[b]][z,,] <- anom[[a]][[i]][[b]][z,,] / 10
                }
            }
        }
    }
}


## looping goes here

for(loc in unique(ameta$loc)){
    for(mon in unique(ameta$month)){
        for(meth in dynmethods) {
            mnum <- as.numeric(gsub('m','',mon))
            mname <- month.abb[mnum]

            testpt <- c(locmap[loc,], list(pch=23, col="black", bg="red"))

            if(!test){
                mplotdir <- paste(plotdir, mon, loc, sep='/')        
                system(paste("mkdir -p", mplotdir))
                plotbase <- paste(meth, mon, loc, "png", sep='.')
            }

            
            ## which anomalies to plot for baseline & clim
            baseids <- rownames(ameta)[with(ameta, month==mon &
                                                   ( scen=="obs" |
                                                     (scen=="hist" &
                                                      method==meth)))]
            
            gnames <- strsplit(baseids, '.', fixed=TRUE) |> sapply('[',3)
            gnamelist <- list(gcm=gnames, var=NULL, lon=NULL, lat=NULL)

            
            ## baselines
            
            ## bind data to be plotted into array[ens,var,lon,lat]
            basedata <- abind(anom$baseline[baseids], along=0,
                              use.dnns=TRUE, new.names=gnamelist)

            ## range types
            rtype <- c(U850=srange, V850=srange, Q850=zerange,
                       T700=narange, Z700=narange, Z500=narange,
                       U250=narange, V250=srange, A850=zerange,
                       S250=narange)

            ## ranges
            blim <- list()
            for(v in vars){
                blim[[v]] <- rtype[[v]](basedata[,v,,])
            }

            if(test) {
                dev.new(width=15, height=5.5)
            } else {
                png(file=paste0(mplotdir,"/baseline.", plotbase),
                    width=15, height=5.5, units="in", res=120)
            }
            
            main <- paste(meth, "baseline upper atmosphere climatology,",
                          "hist", loc)
            gridmap(lon, lat, basedata, mapcol='black', zlims=blim,
                    cmaps=climap, units=uaunits, main=main)
            
            if(!test){ dev.off() }

            
            ## combined monthly climatology plot

            mondata <- abind(anom$clim[baseids], along=0,
                             use.dnns=TRUE, new.names=gnamelist)
            
            mlim <- list()
            for(v in vars){
                mlim[[v]] <- rtype[[v]](mondata[,v,,])
            }
            
            
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
                png(file=paste0(mplotdir,"/monclim.", plotbase),
                    width=10, height=9, units="in", res=120)
            }

            main <- paste(meth, mname, "upper atmosphere climatology,",
                          "hist", loc)
            gridmap(lon, lat, mondata, facets, cmaps=climap, zlims=mlim,
                    units=uaunits, main=main, mapcol="black")

            if(!test) {dev.off()}

            ## monthly anomaly vs baseline climatology
            
            anomdata <- abind(anom$anom[baseids], along=0,
                              use.dnns=TRUE, new.names=gnamelist)

            alim <- list()
            for(v in vars){
                alim[[v]] <- srange(anomdata[,v,,])
            }

            if(test) {
                dev.new(width=10, height=9)
            } else {            
                png(file=paste0(mplotdir,"/anomaly.", plotbase),
                    width=10, height=9, units="in", res=120)
            }
            
            main <- paste(mname, "upper atmosphere anomaly vs climatology,",
                          "hist", loc)
            gridmap(lon, lat, anomdata, facets, cmaps=anomap, zlims=alim,
                    units=uaunits, main=main, mapcol="black",
                    arrowcol="darkgray")
            
            if(!test){ dev.off() }

            
            ## bucketized climatology
            ## delta: by GCM, scen, method; method x vars

            ## short titles
            bfacets <- facets
            bfacets$title <- c("850-mb Qflux",
                               "700-mb T & Z",
                               "high circul'n")

            ## names of anomalies to plot
            baseids <- rownames(ameta)[with(ameta, month==mon &
                                                   ( scen=="obs" |
                                                     (scen=="hist" &
                                                      method==meth)))]
            
            
            ## all methods by bucket

            if(meth == "raw") {
                bmethods <- c("raw", statmethods)
            } else {
                bmethods <- meth
            }

            for(bmeth in bmethods){
                
                bpbase <- sub("raw", bmeth, plotbase)
                bbaseids <- sub("raw", bmeth, baseids)
                deltadata <- lapply(renest(anom$delta[bbaseids]), abind,
                                    along=0, use.dnns=TRUE,
                                    new.names=gnamelist)
                
                dlim <- list()
                for(v in vars){
                    dlim[[v]] <- lapply(deltadata, \(x){srange(x[,v,,])}) |>
                        range()
                }

                bfreq <- subset(bstat, month == mnum & locname == loc &
                                       method %in% c("gridMET", bmeth) &
                                       scen %in% c("obs", "hist"))
                
                
                ## delta (bucket anom - month anom)
                for(b in buckets){
                    
                    if(test) {
                        dev.new(width=10, height=9)
                    } else {
                        png(file=paste0(mplotdir,"/delta.", b, ".", bpbase),
                            width=10, height=9, units='in', res=120)
                    }
                    
                    main <- paste(bmeth, mname, "UA", b,
                                  "anomaly difference,", "hist", loc)

                    gridmap(lon, lat, deltadata[[b]], bfacets, cmaps=anomap,
                            zlims=dlim, units=uaunits, main=main,
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
        } ## meth
    }  ## mon
}  ## loc

stop()

        
## all buckets by method

#testpt <- list(x=x, y=y, pch=23, col="black", bg="red")

for(m in methods){
#    dev.new(width=14, height=9)
    png(file=paste0(mplotdir,"/anom.method.",mm,".",m,".png"),
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

# 
# 
# ## faceted approach using ggplot
# 
# library(ggplot2)
# library(reshape2)  
# library(maps)
# library(plyr)
# library(gridExtra)
# 
# defactor <- function(x) {as.numeric(levels(x))[x]}
# 
# ## ggplot wants data in long format
# ## it's 6x bigger, but not a problem at the monthly scale
# 
# totclim <- abind(uaconus$totclim[ocf], along=0, use.dnns=TRUE) |>
#      setname("period", "ndn") |>
#     adply(c(1,3,4,5))
# 
# ## we do need to convert lat & lon back to numeric
# totclim$lon <- defactor(totclim$lon)
# totclim$lat <- defactor(totclim$lat)
# 
# rawmay <- subset(totclim, month=="May")
# 
# 
# ## wind polygons
# 
# w850scale <- min(diff(clon),diff(clat))/max(mlim$U850, mlim$V850)
# w250scale <- min(diff(clon),diff(clat))/max(mlim$U250, mlim$V250)
# 
# w850 <- with(rawmay, polyfield(U850, V850, lon, lat, scale=w850scale, pad=FALSE))
# w250 <- with(rawmay, polyfield(U250, V250, lon, lat, scale=w250scale, pad=FALSE))
# 
# foo <- mapply(cbind, id=1:length(w850), w850, SIMPLIFY=FALSE) 
# bar <- do.call(rbind, foo)
# baz <- data.frame(id=1:nrow(rawmay), value=rawmay$A850)
# buz <- merge(bar, baz, by=c("id"))
# 
# ggplot(buz, aes(x=x, y=y, asp=1)) + 
#     geom_polygon(aes(fill=value, group=id))+
#     borders("state", xlim=xr, ylim=yr, lwd=1/8, col="black")
# 
# 
# base <- ggplot(rawmay, aes(x=lon, y=lat)) +
#     xlim(xr) + ylim(yr) +
#     coord_fixed() + ## square aspect ratio 
#     theme(legend.position="bottom",
#           axis.title.x = element_blank(),
#           axis.title.y = element_blank()) +
#     borders("state", xlim=xr, ylim=yr, lwd=1/8, col="black")+
#     borders("world",c("Can","Mex"), xlim=xr, ylim=yr, lwd=1/8, col="black")
# 
# qflux <- base + 
#     geom_tile(aes(fill=A850)) + ## raster plot
#     scale_fill_gradientn(colors=climap[["A850"]],
#                          limits=blim[["A850"]]) +
#     facet_grid(vars(period)) +
#     geom_segment(aes(xend=lon+U850*w850scale, yend=lat+V850*w850scale))
# 
# 
# hicirc <- base + 
#     geom_tile(aes(fill=Z500)) + ## raster plot
#     scale_fill_gradientn(colors=anomap[["Z500"]],
#                          limits=blim[["Z500"]])+
#     facet_grid(vars(period)) +
#     geom_segment(aes(xend=lon+U250*w250scale, yend=lat+V250*w250scale))
# 
# grid.arrange(qflux, hicirc, nrow=1)
# 
# 
# ## Okay, that's pretty slick.
# 
# ## Have to figure out how to rearrange layers, but I think it's good.


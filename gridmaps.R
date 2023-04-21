## Make grid plots of climatology, anomaly, etc.

library(abind)

source("calcfun.R")
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


################
## Uniform scales for plotting

## range types for abs vars
rtype <- c(U850=srange, V850=srange, Q850=zerange, T700=narange, Z700=narange,
           Z500=narange, U250=zerange, V250=srange, A850=zerange, S250=zerange)


recrange <- function(x){
    lapply(x, apply, 1, narange) |> do.call(what=rbind) |> apply(2,range)
}

temp <- c(renest(anom$bclim), anom[c("baseline","clim")]) |>
    lapply(recrange) |> do.call(what=rbind) |> apply(2, range)

abslim <- mapply(do.call, as.list(rtype), apply(temp, 2, list)) |>
    as.data.frame() |> as.list()

## override, b/c apparently there's an outlier somewhere:
abslim$A850 <- c(0,60)

#difflim <- c(renest(anom$delta), renest(anom$banom), list(anom$anom)) |> 
#    lapply(recrange) |> do.call(what=rbind) |> apply(2, srange) |>
#    as.data.frame() |> as.list()

## HACK - need to separate current these more (current/future?) and I
## don't have time to figure it out right now.  Hard-coding.

deltalim  <- list(A850=c(-6,6),   T700=c(-2,2), S250=c(-10,10), Z700=c(-25,25))
anomlim   <- list(A850=c(-12,12), T700=c(-5,5), S250=c(-12,12), Z700=c(0,60))
changelim <- list(A850=c(-45,45), T700=c(-8,8), S250=c(-8,8),   Z700=c(30,80))

#################
## analysis loops

for(loc in unique(ameta$loc)){
    testpt <- c(locmap[loc,], list(pch=23, col="black", bg="red"))

    for(mon in unique(ameta$month)){
        mnum <- as.numeric(gsub('m','',mon))
        mname <- month.abb[mnum]

        for(meth in dynmethods) {

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
                png(file=paste0(mplotdir,"/baseline.", plotbase),
                    width=15, height=5.5, units="in", res=120)
            }
            
            main <- paste(meth, "baseline upper atmosphere climatology,",
                          "hist", loc)
            gridmap(lon, lat, basedata, mapcol='black', zlims=abslim, #zlims=blim,
                    cmaps=climap, units=uaunits, main=main)
            
            if(!test){ dev.off() }

            
            ##########
            ## monthly climatology
            
            mondata <- abind(anom$clim[baseids], along=0,
                             use.dnns=TRUE, new.names=gnamelist)

            
            ###########
            ## combined monthly climatology plot
            
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
            gridmap(lon, lat, mondata, facets, cmaps=climap, zlims=abslim, #mlim,
                    units=uaunits, main=main, mapcol="black")

            if(!test) {dev.off()}
            
            ###########
            ## monthly climatology change

            futids <- rownames(ameta)[with(ameta, month==mon & scen=="rcp85" &
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
                png(file=paste0(mplotdir,"/change.", plotbase),
                    width=10, height=7, units="in", res=120)
            }            
            
            main <- paste(meth, mname, "climatology change,", loc,",",
                          uameta$span[3],"vs", uameta$span[2])
            gridmap(lon, lat, changedata, facets, cmaps=anomap, zlims=changelim,
                    units=uaunits, main=main, mapcol="black",
                    arrowcol="darkgray")
            
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
                png(file=paste0(mplotdir,"/anomaly.", plotbase),
                    width=10, height=9, units="in", res=120)
            }
            
            main <- paste(meth, mname, "upper atmosphere anomaly vs climatology,",
                          "hist", loc)
            gridmap(lon, lat, anomdata, facets, cmaps=anomap, zlims=anomlim,
                    units=uaunits, main=main, mapcol="black",
                    arrowcol="darkgray")
            
            if(!test){ dev.off() }


            #############
            ## bucketized climatology

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


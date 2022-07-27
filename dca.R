Library(devtools)
load_all("~/climod")

files <- dir('prec', "*nc", full=TRUE)
nc <- lapply(files, nc_open)
M <- strsplit(files,'.',TRUE) |> sapply(`[`, 4)

data <- lapply(nc, nc_ingest) |> setNames(M)

prec <- lapply(data, `[[`, "prec")

## reorder methods by type

mods <- c("raw","RegCM4","WRF","MPAS","CNN","LOCA","MBCn","KDDM","SDSM")

prec <- prec[mods]

## MPAS and WRF are missing 4 leap days; insert NA values
# lapply(data, `[[`, "time") |> lapply(attributes) |> sapply(`[[`, "calendar")
ileap <- c(2,6,10,14)*365 + 21 + 28 + c(0,1,2,3)
for(i in rev(ileap)){
    prec$MPAS <- append(prec$MPAS, NA, i)
    prec$WRF  <- append(prec$WRF,  NA, i)
}

## CNN has a bunch of NA values, but nothing exactly 0.  Set NA to 0
prec$CNN[is.na(prec$CNN)] <- 0


#### this goes elsewhere...
goodct <- sapply(prec, function(x){sum(is.finite(x))})

######## Need to subset by month hereabouts


########

## convert to matrix

pmat <- do.call(cbind, prec)^(1/4)

rg <- range(pmat, na.rm=TRUE)


## timeseries plot
# dev.new()
# matplot(pmat, ylim=rg, pch='.')


## bucket barchart

trace <- 0.254
theta <- 3
trace4 <- trace^(1/4)
theta4 <- theta^(1/4)

dmw <- c("dry", "moist", "wet")

cats <- apply(pmat, 2, cut, breaks=c(0, trace4, theta4, Inf),
              labels=dmw, include.lowest=TRUE)

catfrac <- apply(cats, 2, table) / goodct


dev.new()

bmap <- c(dry=7, moist=3, wet=4)
    
barplot(catfrac*100, col=bmap, las=2, legend=FALSE, ylab='%',
        main="Precip category fractions")

legend(bg="white", "bottomright", seg.len=1, box.col="white",
       c("wet", "3 mm", "moist", '0.01"', "dry"),
       fill=c(4,NA,3,NA,7),
       border=c(1,NA,1,NA,1),
       lty=c(NA,1,NA,1,NA),
       x.intersp=c(0,1,0,1,0))


## dist plots, comparison to raw

# clean <- function(x){x[is.finite(x) & x >= trace4]}
#
# rng <- function(...){range(..., na.rm=TRUE)}
# 
# corrplot <- function(x, y, pch='.', xlab='', ylab='', xylim=rng(x,y), ...){
#     plot(x, y, pch=pch, xlim=xylim, ylim=xylim, xlab=xlab, ylab=ylab, ...)
#     fit <- lm(y ~ x, list(x,y))
#     abline(fit, col='blue')
#     rho = format(cor(x,y,method="pearson", use="complete"), digits=3)
#     tau = format(cor(x,y,method="kendall", use="complete"), digits=3)
#     mtext(bquote(rho == .(rho)), side=3, line=-1.5, adj=0.1, cex=0.65)
#     mtext(bquote(tau == .(tau)), side=1, line=-1.5, adj=0.9, cex=0.65)
# }
# 
#         
# qqplot <- function(x, y, pch='.', xlab='', ylab='', xylim=rng(x,y), ...){
#     xx <- sort(x, na.last=FALSE)
#     yy <- sort(y, na.last=FALSE)
#     plot(xx, yy, xlim=xylim, ylim=xylim, pch=pch, xlab=xlab, ylab=ylab, ...)
#     abline(0,1,col='red')
# }
#
# tblack = adjustcolor("black",alpha=1/5)



dev.new(width=14, height=6)

par(mfcol=c(4,length(mods)), mar=c(2,2,1.5,1), oma=c(0,2,2,0), mgp=c(2,0.5,0))

for(m in mods){
    distplots(pmat[,'raw'], pmat[,m], lower=trace4, xylim=narange(pmat))
}

# praw <- pmat[,'raw']

# for(m in mods){    
# 
#     pmod <- pmat[,m]
#     p <- clean(pmod)
# 
#     corrplot(praw, pmod, col=tblack, xylim=rg, main=m)
# 
#     qqplot(praw, pmod, xylim=rg)
#     abline(v=trace4, h=trace4, col='gray', lty=2)
#     abline(v=1, h=1, col='gray', lty=3)
# 
#     hist(p, main='', xlab='', ylab='', xlim=rg,
#          breaks=seq(trace4, max(rg), length=15))
#     abline(v=trace4, col='gray', lty=2)
# 
#     plot(density(p), xlab='', ylab='', main='', xlim=rg)
#     abline(v=trace4, col='gray', lty=2)
# }
mtext(outer=TRUE, side=2, c("pdf","hist","Q-Q","corr"), at=1:4/4-1/8)
mtext(outer=TRUE, side=3, "MPI downscaling distributions for 4th-root precip")


#########################
## upper atmosphere climatology

## kinda hacky, but

year <- 1990:2005
leap <- year %% 4 == 0
doys <- unlist(lapply(leap, function(ly){
    if(ly){c(1:59,59.5,60:365)}
    else{1:365}
    }))
monlens <- c(31,28,31,30,31,30,31,31,30,31,30,31) |> setNames(month.abb)
monends <- cumsum(monlens)
month <- cut(doys, c(0,monends), labels=month.abb)

####

uav <- c("U850", "V850", "Q850", "T700", "Z700", "Z500", "U250", "V250")
uavar <- substr(uav,1,1)
plev <- paste0("p",substr(uav,2,4))

uafiles <- paste0("ua/", uavar, ".MPI-ESM-LR.hist.CONUS.", plev,
                  ".1990-2005.dayavg.mpigrid.nc") |> setNames(uav)

uanc <- lapply(uafiles, nc_ingest)

lon <- uanc[[1]]$lon
lat <- uanc[[1]]$lat

uadata <- mapply(`[[`, uanc, uavar, SIMPLIFY=FALSE) |> setNames(uav)
uadata$Q850 <- uadata$Q850 * 1000
uadata$Q850@units = "g/kg"

units <- sapply(uadata, "@", "units") |> gsub(pat=" s-1", rep="/s")

## Here... calc clim split by month

library(abind)

nx <- length(lon)
ny <- length(lat)
nm <- length(mods)
nv <- length(uav)

#uamonth <- array(dim=c(12,nv,nx,ny)
#               dimnames=list(month=month.abb, var=uav, lon=NULL, lat=NULL))
#


## days per month is ragged, so we still lists until we aggregate to clim

## list by month of ua[var(8), lon(43), lat(21), mday(448~500)]
uamon <- list()
for(m in month.abb){
    uamon[[m]] <- lapply(uadata, function(x){x[,,month==m]}) |> abind(along=0)
}

## ua[month(12), var(8), lon(43), lat(21)]
monclim <- lapply(uamon, apply, 1:3, mean) |> abind(along=0)

## list by month of dmw[model(9), mday(448~500)]
moncats <- split.data.frame(cats, month) |> lapply(t)

## ua[month(12), dmw(3), model(9), var(8), lon(43), lat(21)}

mondmwclim <- array(dim=c(12, 3, nm, nv, nx, ny),
               dimnames=list(month=month.abb, cat=dmw, model=mods, var=uav, lon=NULL, lat=NULL))


for(m1 in month.abb){
    for(d in dmw){
        for(m2 in mods){
            mondmwclim[m1,d,m2,,,] <- uamon[[m]][,,,moncats[[m]][m2,]==d] |>
                apply(1:3, mean)
        }
    }
}



    
#may <- lapply(uadata, function(x){x[,,month=="May"]})

#clim <- lapply(may, apply, 1:2, mean)

#zrc <- list(
#    U850 = c(-10,10),
#    V850 = c(-10,10),
#    Q850 = c(0,15),
#    T700 = c(260,285),
#    Z700 = c(2900,3200),
#    Z500 = c(5400,5900),
#    U250 = c(-30,30),
#    V250 = c(-30,30)
#)

source("cmaps.R")

#library(pals)
#library(colorspace)
#
#N <- 256
#
#cmap <- list(
#    U850=brewer.piyg(N),
#    V850=brewer.puor(N),
#    Q850=rev(cubehelix(N)),
#    T700=inferno(N),
#    Z700=gnuplot(N),
#    Z500=brewer.spectral(N),
#    U250=colorRampPalette(c("blue","red","green"))(N),
#    V250=brewer.puor(N)
#    )  
#
#dmap <- list(
#    U850=brewer.piyg(N),
#    V850=brewer.puor(N),
#    Q850=brewer.brbg(N),
#    T700=brewer.rdbu(N),
#    Z700=brewer.spectral(N),
#    Z500=brewer.spectral(N),
#    U250=brewer.piyg(N),
#    V250=brewer.puor(N)
#    )
#
### for adding colorbars using image()
#cbar <- t(matrix(rep(1:N, each=3), nrow=3))
#cx <- seq(0,1,len=N)
#dx <- seq(-1,1, len=N)
#
#library(fields)
#library(maps)


#
#par(mfrow=c(4,8), oma=c(0,3,2,0), mar=c(3,1,1,1), mgp=c(2,0.5,0))
#
#
#
#xr <- c(-125,-75)
#yr <- c(20,60)
#
#for(v in uav){
#    image.plot(lon, lat, clim[[v]], col=cmap[[v]], horizontal=TRUE,
#               ann=FALSE, xlim=xr, ylim=yr, zlim=zrc[[v]])
#    title(v, line=1, cex=2, xpd=NA)
#    world(add=TRUE)
#    US(add=TRUE)
#}
#
#mtext(c("clim",dmw), side=2, outer=TRUE, line=1.25, at=4:1/4-1/8)
#
### calculate & plot clim patterns by category

#maydmw <- cats[month=="May","raw"]

#delta <- list()
pats <- list()

#library(abind)

for(d in dmw){    
    pats[[d]] <- abind(may, along=0)[,,,maydmw==d] |>
        apply(1:3, mean) |> asplit(1)
#    delta[[d]] <- mapply(`-`, pats[[d]], clim, SIMPLIFY=FALSE)
}


# for(d in dmw){    
#     for(v in uav){
#         image.plot(lon, lat, pats[[d]][[v]], col=cmap[[v]], horizontal=TRUE,
#                    ann=FALSE, xlim=xr, ylim=yr, zlim=zrc[[v]])
#         title(v, line=1, cex=2, xpd=NA)
#         world(add=TRUE)
#         US(add=TRUE)
#     }
# }

## above here, convert clim  from list to array

zrng <- apply(clim, 3, range) |> abs() |> apply(2, max) # |> ceiling() 

source("plotfun.R")

dev.new(width=12, height=6)

gridmap(lon, lat, clim, cmaps=cmap, zlims=zrng, units=units, main="MPI May climatology")


### Deltas from climatology

## calculate

# nx <- length(lon)
# ny <- length(lat)
# nm <- length(mods)
# nv <- length(uav)

delta <- array(dim=c(3, nm, nv, nx, ny),
               dimnames=list(dmw=dmw, model=mods, var=uav, lon=NULL, lat=NULL))

for(d in dmw){
    for(m in mods){
        maydmw <- cats[month=="May",m]
        mdpat <- abind(may, along=0)[,,,maydmw==d] |> apply(1:3, mean)
        for(v in uav){
            delta[d,m,v,,] <- mdpat[v,,] - clim[[v]]
        }
    }
}

# drg <- c(U850=5, V850=5, Q850=2, T700=2.5, Z700=25, Z500=40, U250=10, V250=10)

drg <- apply(delta, 3, range) |> abs() |> apply(2, max) # |> ceiling()
dlims <-  lapply(drg, `*` ,c(-1,1))



## Plot deltas

## Note: rendering slows down with each panel.  Not always true, but
## not dependent on code as far as I can tell.

# for(d in dmw){
#     dev.new(width=nv*1.9+1, height=nm+1)
# 
#     par(mfrow=c(nm,nv), oma=c(5,2,4,2), mar=c(1,1,1,1)/4, mgp=c(3,1/2,0))
#     
#     for(m in mods){
#         for(v in uav){
#             image(lon, lat, delta[d,m,v,,], zlim=drg[v]*c(-1,1),
#                   col=dmap[[v]], xaxt='n', yaxt='n', ann=FALSE)
#             if(m==mods[1]) title(v, line=1, cex=2, xpd=NA)
#             if(v==uav[nv]) axis(4)
#             if(m==mods[nm]) axis(1)
#             map("state", col='darkgray', add=TRUE, lwd=1/2)
#             map("world", c("can","mex"), col='darkgray', add=TRUE, lwd=1/2)
#         }
#     }
#     mtext(mods, side=2, outer=TRUE, line=0.5, at=(nm:1 - 1/2)/nm)
#     mtext(paste(d,"day average anomaly vs MPI May climatology"), side=3, line=2, outer=TRUE)
# 
#     par(new=TRUE, oma=c(0,2,4,2), mar=c(2,2,1,1), mgp=c(3,1/2,0),
#         mfrow=c(nm*2, nv), mfg=c(nm*2,1))
#     for(v in uav){   
#         image(x=dx*drg[v], z=cbar, col=dmap[[v]], yaxt='n')
#         mtext(units[v], side=2, las=2, cex=0.7, line=1/4)
#     }
# }

## A function to create a grid of closely-tiled image plots with
## common lat/lon axes for a dataset with a variable dimension and an
## ensemble dimension. Plots are generated using image(), and then
## geographic boundaries are overlaid using map().  A colorbar (with
## units string) is plotted at the bottom of each column.

## @param x,y,z: x, y, and z values to be passed to image().  z should
## be an array dimensioned ensemble, variable, lon, lat.  row/colnames
## on the ensemble and variable dimensions are used for row/column
## headers.  If x/y are not lon/lat values, be sure to disable map
## overlays by setting dbs to NULL.

## @param cmaps: a named list of colormaps, one per variable.  When
## unspecified, viridis(256) is used as a default.

## @param zlims: a named list of zlim values, one per variable.  When
## unspecified, the range of the data for that variable across the
## entire ensemble is used.

## @param units: a named vector of units for each variable, used to
## annotate colormaps.

## @param dbs: a list of geographical databases (e.g., "state" for US
## states) to be overlaid using map().  Overlays are skipped if this
## argument is NULL.  Defaults to North America (US states + Canada &
## Mexico).

## @param regs: a list of regions to be overlaid for each database in
## dbs.  Use '.' to get all regions in the database.

## @param mapcol: color for map overlay lines

## @param lwd: line width for map overlay

## @param main: main title

## @param params: a list of named arguments to pass to par().  Don't
## use mfrow or mfg, which are computed internally.  mar determines
## spacing between individual panels; spacing for annotations and axes
## are accommodated by oma, which also includes extra space at the
## bottom for colorbars.

## @param cbarmar: margins for colorbars at the bottom

## @param ...: other arguments passed to image()

# gridmap <- function(x, y, z, cmaps=NULL, zlims=NULL,
#                     units=NULL, main=NULL,
#                     dbs=list("state","world"),
#                     regs=list(".", c("Can","Mex")),
#                     mapcol='darkgray', lwd=1/2,
#                     params=list(oma=c(5,2,4,2),
#                         mar=c(1,1,1,1)/4,
#                         mgp=c(3,1/2,0)),
#                     cbarmar=c(2,2,1,1), ...)  {
# 
#     mods <- dimnames(z)[[1]]
#     vars <- dimnames(z)[[2]]
#     nm <- length(mods)
#     nv <- length(vars)
# 
#     params$mfrow <- c(nm, nv)
#     do.call(par, params)
#     
#     for(m in mods){
#         for(v in vars){
#             image(lon, lat, z[m,v,,], zlim=zlims[[v]], col=cmaps[[v]],
#                   xaxt='n', yaxt='n', ann=FALSE)
#             if(m==mods[1]) title(v, line=1, cex=2, xpd=NA)
#             if(m==mods[nm]) axis(1)
#             if(v==vars[nv]) axis(4)
#             mapply(map, dbs, regs, MoreArgs=list(add=TRUE, lwd=lwd, col=mapcol))
#         }
#     }
#     mtext(mods, side=2, outer=TRUE, line=0.5, at=(nm:1 - 1/2)/nm)
#     mtext(main, side=3, line=2, outer=TRUE)
# 
#     ## colorbars
#     par(new=TRUE, oma=c(0, params$oma[-1]), mar=c(2,2,1,1),
#         mgp=c(3,1/2,0), mfrow=c(nm*2, nv), mfg=c(nm*2,1))
# 
#     for(v in vars){
#         cbmap <- cmaps[[v]]
#         N <- length(cbmap)
#         cbx <- do.call(seq, as.list(c(zlims[[v]], len=N)))
#         cbz <- t(matrix(rep(1:N, each=3), nrow=3))
#         
#         image(x=cbx, z=cbz, col=cbmap, yaxt='n')
#         mtext(units[v], side=2, las=2, cex=0.7, line=1/4)
#     }    
# }
# 
for(d in dmw){
    dev.new(width=nv*1.9+1, height=nm+1)
    dtitle <- paste(d,"day average anomaly vs MPI May climatology")
    gridmap(lon, lat, delta[d,,,,], cmaps=dmap, zlims=dlims, main=dtitle, units=units)
}


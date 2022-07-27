library(maps)

narange <- function(...){range(..., na.rm=TRUE)}

srange <- function(...){c(-1,1)*max(narange(...))}

## plot that uses symmetric ranges for both axes
symplot <- function(xylim=narange(x, y), ...){
    plot(xlim=xylim, ylim=xylim, ...)
}

fade <- function(color, x){
    adjustcolor(color, alpha=1/log10(length(x)))
}

corrplot <- function(x, y, pch='.',
                     ptcolor=fade(1, length(x)),
                     fitcolor=3, digits=3, ...){
    stopifnot(length(x)==length(y))
    symplot(x, y, pch=pch, col=ptcolor, ...)
    fit <- lm(y ~ x, list(x,y))
    abline(fit, col=fitcolor)
    rho = cor(x, y, method="pearson", use="complete") |> format(digits=digits)
    tau = cor(x, y, method="kendall", use="complete") |> format(digits=digits)
    mtext(bquote(rho == .(rho)), side=3, line=-1.5, adj=0.1, cex=0.65)
    mtext(bquote(tau == .(tau)), side=1, line=-1.5, adj=0.9, cex=0.65)
}

        
qqplot <- function(x, y, pch='.', idcolor=6, ...){
    stopifnot(length(x)==length(y))
    xx <- sort(x, na.last=FALSE)
    yy <- sort(y, na.last=FALSE)
    symplot(xx, yy, pch=pch, ...)
    abline(0, 1, col=idcolor)
}


## double histogram

hist2 <- function(x, y, xcolor=2, ycolor=4, xlim=narange(x,y), ...){
    bk = seq(min(x,y), max(x,y), length=15)
    hist(x, xlim=xlim, col=adjustcolor(xcolor, 1/4), breaks=bk, ...)
    hist(y, xlim=xlim, col=adjustcolor(ycolor, 1/4), breaks=bk, add=TRUE, ...)
}

## dual density plot

ddplot <- function(x, y, xcolor=2, ycolor=4, xlim=narange(x, y), ...){
    dx <- density(x)
    dy <- density(y)

    plot(NA, xlim=xlim, ylim=c(0, max(dx$y, dy$y)), ...)
    lines(dx, col=xcolor)
    lines(dy, col=ycolor)
}

clean <- function(x, lower=-Inf, upper=Inf){
    x[is.finite(x) & x >= lower & x <= upper]
}


## args to pass in if they exist instead of hardcoding:
## corrplot: fitcolor, ptcolor
## qqplot: idcolor
## hist2, ddplot: xcolor, ycolor
## lower: ?

distplots <- function(x, y, xylim=narange(x,y),
                      lower=-Inf, upper=Inf, bins=15, ...){

    corrplot(x, y, xylim=xylim, ann=FALSE)

    qqplot(x, y, xylim=xylim, ann=FALSE)
    abline(v=lower, h=lower, col='gray', lty=2)
    abline(v=upper, h=upper, col='gray', lty=3)

    xx <- clean(x, lower=lower, upper=upper)
    yy <- clean(y, lower=lower, upper=upper)
    
    hist2(xx, yy, xlim=xylim, main='', ann=FALSE)
    abline(v=c(lower, upper), col='gray', lty=c(2,3))
    
    ddplot(xx, yy, ann=FALSE)
    abline(v=c(lower, upper), col='gray', lty=c(2,3))
}

# stop()

#########################
## upper atmosphere climatology

## kinda hacky, but

#year <- 1990:2005
#leap <- year %% 4 == 0
#doys <- unlist(lapply(leap, function(ly){
#    if(ly){c(1:59,59.5,60:365)}
#    else{1:365}
#    }))
#monlens <- c(31,28,31,30,31,30,31,31,30,31,30,31) |> setNames(month.abb)
#monends <- cumsum(monlens)
#month <- cut(doys, c(0,monends), labels=month.abb)
#
####
#
#uav <- c("U850", "V850", "Q850", "T700", "Z700", "Z500", "U250", "V250")
#uavar <- substr(uav,1,1)
#plev <- paste0("p",substr(uav,2,4))
#
#uafiles <- paste0("ua/", uavar, ".MPI-ESM-LR.hist.CONUS.", plev,
#                  ".1990-2005.dayavg.mpigrid.nc") |> setNames(uav)
#
#uanc <- lapply(uafiles, nc_ingest)
#
#lon <- uanc[[1]]$lon
#lat <- uanc[[1]]$lat
#
#uadata <- mapply(`[[`, uanc, uavar, SIMPLIFY=FALSE) |> setNames(uav)
#uadata$Q850 <- uadata$Q850 * 1000
#uadata$Q850@units = "g/kg"
#
#units <- sapply(uadata, "@", "units") |> gsub(pat=" s-1", rep="/s")
#
#may <- lapply(uadata, function(x){x[,,month=="May"]})
#
#clim <- lapply(may, apply, 1:2, mean)
#
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
#
#
#
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
# dev.new(width=12, height=6)
# 
# par(mfrow=c(4,8), oma=c(0,3,2,0), mar=c(3,1,1,1), mgp=c(2,0.5,0))
# 
# xr <- c(-125,-75)
# yr <- c(20,60)
# 
# for(v in uav){
#     image.plot(lon, lat, clim[[v]], col=cmap[[v]], horizontal=TRUE,
#                ann=FALSE, xlim=xr, ylim=yr, zlim=zrc[[v]])
#     title(v, line=1, cex=2, xpd=NA)
#     world(add=TRUE)
#     US(add=TRUE)
# }
# 
# mtext(c("clim",dmw), side=2, outer=TRUE, line=1.25, at=4:1/4-1/8)
# 
# ### calculate & plot clim patterns by category
# 
# maydmw <- cats[month=="May","raw"]
# 
# #delta <- list()
# pats <- list()
# 
# library(abind)
# 
# for(d in dmw){    
#     pats[[d]] <- abind(may, along=0)[,,,maydmw==d] |>
#         apply(1:3, mean) |> asplit(1)
# #    delta[[d]] <- mapply(`-`, pats[[d]], clim, SIMPLIFY=FALSE)
# }
# 
# 
# for(d in dmw){    
#     for(v in uav){
#         image.plot(lon, lat, pats[[d]][[v]], col=cmap[[v]], horizontal=TRUE,
#                    ann=FALSE, xlim=xr, ylim=yr, zlim=zrc[[v]])
#         title(v, line=1, cex=2, xpd=NA)
#         world(add=TRUE)
#         US(add=TRUE)
#     }
# }
# 
# ### Deltas from climatology
# 
# ## calculate
# 
# nx <- length(lon)
# ny <- length(lat)
# nm <- length(mods)
# nv <- length(uav)
# 
# delta <- array(dim=c(3, nm, nv, nx, ny),
#                dimnames=list(dmw=dmw, model=mods, var=uav, lon=NULL, lat=NULL))
# 
# for(d in dmw){
#     for(m in mods){
#         maydmw <- cats[month=="May",m]
#         mdpat <- abind(may, along=0)[,,,maydmw==d] |> apply(1:3, mean)
#         for(v in uav){
#             delta[d,m,v,,] <- mdpat[v,,] - clim[[v]]
#         }
#     }
# }
# 
# # drg <- c(U850=5, V850=5, Q850=2, T700=2.5, Z700=25, Z500=40, U250=10, V250=10)
# 
# drg <- apply(delta, 3, range) |> abs() |> apply(2, max) # |> ceiling()
# dlims <-  lapply(drg, `*` ,c(-1,1))
# 
# 

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

gridmap <- function(x, y, z, cmaps=NULL, zlims=NULL,
                    units=NULL, main=NULL,
                    dbs=list("state","world"),
                    regs=list(".", c("Can","Mex")),
                    mapcol='darkgray', lwd=1/2,
                    params=list(oma=c(5,2,4,2),
                        mar=c(1,1,1,1)/4,
                        mgp=c(3,1/2,0)),
                    cbarmar=c(2,2,1,1), ...)  {

    mods <- dimnames(z)[[1]]
    vars <- dimnames(z)[[2]]
    nm <- length(mods)
    nv <- length(vars)

    params$mfrow <- c(nm, nv)
    do.call(par, params)
    
    for(m in mods){
        for(v in vars){
            image(lon, lat, z[m,v,,], zlim=zlims[[v]], col=cmaps[[v]],
                  xaxt='n', yaxt='n', ann=FALSE)
            if(m==mods[1]) title(v, line=1, cex=2, xpd=NA)
            if(m==mods[nm]) axis(1)
            if(v==vars[nv]) axis(4)
            mapply(map, dbs, regs, MoreArgs=list(add=TRUE, lwd=lwd, col=mapcol))
        }
    }
    mtext(mods, side=2, outer=TRUE, line=0.5, at=(nm:1 - 1/2)/nm)
    mtext(main, side=3, line=2, outer=TRUE)

    ## colorbars
    par(new=TRUE, oma=c(0, params$oma[-1]), mar=c(2,2,1,1),
        mgp=c(3,1/2,0), mfrow=c(nm*2, nv), mfg=c(nm*2,1))

    for(v in vars){
        cbmap <- cmaps[[v]]
        N <- length(cbmap)
        cbx <- do.call(seq, as.list(c(zlims[[v]], len=N)))
        cbz <- t(matrix(rep(1:N, each=3), nrow=3))
        
        image(x=cbx, z=cbz, col=cbmap, yaxt='n')
        mtext(units[v], side=2, las=2, cex=0.7, line=1/4)
    }    
}

# for(d in dmw){
#     dev.new(width=nv*1.9+1, height=nm+1)
#     dtitle <- paste(d,"day average anomaly vs MPI May climatology")
#     gridmap(lon, lat, delta[d,,,,], cmaps=dmap, zlims=dlims, main=dtitle, units=units)
# }
# 
# 

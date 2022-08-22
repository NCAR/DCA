library(maps)
source("util.R")

## plot that uses symmetric ranges for both axes
symplot <- function(xylim=narange(x, y), ...){
    plot(xlim=xylim, ylim=xylim, ...)
}


## autoadjusting color for scatter plots with manymany points
fade <- function(color, x){
    adjustcolor(color, alpha=1/log10(length(x)))
}


## symmetric scatterplot with linear fit
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


## Q-Q plot with symmetric axes & identity line        

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


## args to pass in if they exist instead of hardcoding:
## corrplot: fitcolor, ptcolor
## qqplot: idcolor
## hist2, ddplot: xcolor, ycolor
## lower: ?

## 4 plots: corrplot, qqplot, double histogram, dual density

distplots <- function(x, y, xylim=narange(x,y),
                      lower=-Inf, upper=Inf, bins=15, ...){

    corrplot(x, y, xylim=xylim, ann=FALSE)

    qqplot(x, y, xylim=xylim, ann=FALSE)
    abline(v=lower, h=lower, col='gray', lty=2)
    abline(v=upper, h=upper, col='gray', lty=3)

    xx <- trim(x, lower=lower, upper=upper)
    yy <- trim(y, lower=lower, upper=upper)
    
    hist2(xx, yy, xlim=xylim, main='', ann=FALSE)
    abline(v=c(lower, upper), col='gray', lty=c(2,3))
    
    ddplot(xx, yy, ann=FALSE)
    abline(v=c(lower, upper), col='gray', lty=c(2,3))
}


## Create a grid of closely-tiled image plots with common lat/lon axes
## for a dataset with a variable dimension and an ensemble
## dimension. Plots are generated using image(), and then geographic
## boundaries are overlaid using map().  A colorbar (with units
## string) is plotted at the bottom of each column.  Axes are only on
## the left and bottom plots.

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

## @param main: main title

## @param dbs: a list of geographical databases (e.g., "state" for US
## states) to be overlaid using map().  Overlays are skipped if this
## argument is NULL.  Defaults to North America (US states + Canada &
## Mexico).

## @param regs: a list of regions to be overlaid for each database in
## dbs.  Use '.' to get all regions in the database.

## @param mapcol: color for map overlay lines

## @param maplwd: line width for map overlay

## @param spacing: spacing between gridded map panels, in inches.

## @param cbhi: adjustment to height of the colorbar, in inches.

## @param margi: combined margins for the composite plot, in inches.
## A numerical vector of the form c(gridmap.bottom, left, gridmap.top,
## right, colorbar.bottom, colorbar.top).  These margins should
## include space for annotations and axes.

## @param uniti: amount of space (in inches) to leave for the units
## string to the left of each colorbar

## @param ...: other arguments passed to image()

## Note: don't use parameters oma, omi, mar, mai, mfrow, or mfcol;
## these are used internally to create the plot and will be ignored /
## overwritten.

gridmap <- function(x, y, z,
                    cmaps=NULL, zlims=NULL,
                    units=NULL, main=NULL,
                    dbs=list("state","world"),
                    regs=list(".", c("Can","Mex")),
                    mapcol='darkgray', maplwd=1/2,
                    spacing=c(1,1,1,1)/20, cbhi=0,
                    margi=c(2,3,5,3,2,2)/8, uniti=1/8,
                    ...){

    mods <- dimnames(z)[[1]]
    vars <- dimnames(z)[[2]]
    nm <- length(mods)
    nv <- length(vars)


    ## fill out missing defaults

    ## default: by column
    if(missing(zlims)){
        zlims <- apply(z, 2, narange) |> as.data.frame() |> as.list()
    }

    ## if single range given, reuse for all vars
    if(!is.list(zlims)){
        zlims <- rep(list(zlims), nm) |> setname(vars)
    }

    ## if no cmap given, use rwb palette
        if(missing(cmaps)){
            cmaps <- colorRampPalette(c("red","white","blue"))(256)
        }

    ## if single cmap given, reuse for all vars
    if(!is.list(cmaps)){
        cmaps <- rep(list(cmaps), nm) |> setname(vars)
    }


    ## margin calculations for colorbar space
    
    cbary <- sum(cbhi, margi[5:6])
    
    gridomi <- c(margi[1]+cbary, margi[2:4])
    cbaromi <- c(margi[5], margi[2], par()$din[2]-cbary, margi[4])

    
    ## plotting
    
    mfrow <- c(nm, nv)
    par(mfrow=mfrow, omi=gridomi, mai=spacing)
    
    for(m in mods){
        for(v in vars){
            image(lon, lat, z[m,v,,], zlim=zlims[[v]], col=cmaps[[v]],
                  xaxt='n', yaxt='n', ann=FALSE, ...)
            if(m==mods[1]) title(v, line=1, cex=2, xpd=NA)
            if(m==mods[nm]) axis(1)
            if(v==vars[nv]) axis(4)
            mapply(map, dbs, regs, MoreArgs=list(add=TRUE, lwd=maplwd, col=mapcol))
        }
    }
    mtext(mods, side=2, outer=TRUE, line=0.5, at=(nm:1 - 1/2)/nm)
    mtext(main, side=3, line=2, outer=TRUE)
    
    ## colorbars
    
    par(new=TRUE, omi=cbaromi, mai=c(0,1,0,1)*uniti + spacing,
        mfrow=c(1,nv), mfg=c(1,1))
    
    for(v in vars){
        cbmap <- cmaps[[v]]
        N <- length(cbmap)
        cbx <- do.call(seq, as.list(c(zlims[[v]], len=N)))
        cbz <- t(matrix(rep(1:N, each=3), nrow=3))
        
        image(x=cbx, z=cbz, col=cbmap, yaxt='n')
        mtext(units[v], side=2, las=2, cex=0.7, line=1/4)
    }
}



#######  Moisture advection plot experimentation

# 
# foo <- data$total$baseline$obs
# 
# vscale <- mean(c(diff(lon),diff(lat))) / max(abs(range(foo[c("U850","V850"),,])))
# 
# nx <- length(lon)
# ny <- length(lat)
# lon2d <- rep(lon, each=ny)
# lat2d <- rep(lat, nx)
# 
# u <- foo["U850",,]
# v <- foo["V850",,]
# q <- foo["Q850",,]
# z <- foo["Z700",,]
# 
# 
# dev.new(width=12, height=7)
# 
# image(lon, lat, foo["Q850",,], col=climap[["Q850"]], ylim=yr, xlim=xr)
# map("state", add=TRUE, col='yellow')
# map("world", c("can","mex"), add=TRUE, col='yellow')
# contour(lon, lat, z, add=TRUE, lwd=2, labcex=1)
# arrows(lon2d, lat2d, lon2d+vscale*u, lat2d+vscale*v, length=0.02, lwd=2)
# 
## would need to subset data to xlim/ylim region to get contour labels at edges

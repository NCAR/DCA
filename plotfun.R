library(maps)

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

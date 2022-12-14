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

## @param facets: a dataframe with columns named "raster", "vector",
## "contour", and "title" indicating how to plot different variables
## of the z-array in different columns.  Each row of the dataframe
## corresponds to a column in the grid.  Entries in the "raster"
## "fields" column are the names of variables to plot as rasters
## fields using image(), entries in the "vector" column are pairs of
## variable names overplotted as arrows using vectorfield(). and
## entries in the "contour" column are overplotted using contour().
## If facets$title is NULL, it uses the values of facets$raster as a
## default.  If facets is NULL, it defaults to raster-plotting each
## variable as defined by dimnames(z)$var.

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

## @param halo, hcolor, hwidth: it may be hard to see map borders
## against colormaps that have both light and dark values, like
## cubehelix.  If halo=TRUE, gridmap uses the halomap() function to
## draw map borders twice in contrasting colors to make them show up
## better.  Borders are drawn first in hcolor with lwd=maplwd*hwidth,
## and then re-drawn in mapcol with lwd=maplwd.

## @param arrowcol: color for vector-field arrows.  Default is
## "contrast", which uses the bwcontrast() function to make them black
## where the raster image is light and white where it's dark.

## @param fatten: whether to fatten vector-field arrows based on
## magnitude.  If TRUE, arrow width for minimum value of vector field
## is lwd, and arrow width for the maximum value is double that.  If
## set to a numeric value, adds that many times the line width.  So
## fatten=2 makes arrows up to 3x the basic lwd, and fatten=-1/2 makes
## short arrows thick and long arrows thin.

## @param alen: arrowhead length (default 0.015)

## @param concol: color for contours (default: white)

## @param conlwd: line width for contours (default: 2)

## @param concex: cex (absolute) for contour labels (default: 0.8)

## @param pointargs: a list of arguments to points() added to each panel

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
                    facets=NULL,
                    cmaps=NULL, zlims=NULL,
                    units=NULL, main=NULL,
                    dbs=list("state","world"),
                    regs=list(".", c("Can","Mex")),
                    mapcol='darkgray', maplwd=1/2,
                    halo=FALSE, hcolor="white", hwidth=2,
                    arrowcol="contrast", fatten=FALSE, alen=0.015,
                    concol="white", conlwd=2, concex=0.8,
                    pointargs=NULL,
                    spacing=c(1,1,1,1)/20, cbhi=0,
                    margi=c(2,3,5,3,2,2)/8, uniti=1/8,
                    ...){

    
    ## fill in any missing defaults

    ## cases to plot
    if(missing(facets)){
        facets <- as.data.frame(cbind(raster = dimnames(z)$var))
        rownames(facets) <- facets$raster
    }
    if(is.null(facets$title)){
        facets$title <- facets$raster
    }


    nf <- nrow(facets)
    cases <- dimnames(z)[[1]]
    nc <- length(cases)
    
    
    ## default zlims by column
    if(missing(zlims)){
        zlims <- apply(z, 2, narange) |> as.data.frame() |> as.list()
    }

    ## if single range given, reuse for all facets
    if(!is.list(zlims)){
        zlims <- rep(list(zlims), nm) |> setname(rownames(facets))
    }

    ## if no cmap given, use rwb palette
        if(missing(cmaps)){
            cmaps <- colorRampPalette(c("red","white","blue"))(256)
        }

    ## if single cmap given, reuse for all vars
    if(!is.list(cmaps)){
        cmaps <- rep(list(cmaps), nm) |> setname(rownames(facets))
    }


    ## margin calculations for colorbar space
    
    cbary <- sum(cbhi, margi[5:6])
    
    gridomi <- c(margi[1]+cbary, margi[2:4])
    cbaromi <- c(margi[5], margi[2], par()$din[2]-cbary, margi[4])

    
    ## plotting
    
    mfrow <- c(nc, nf)
    par(mfrow=mfrow, omi=gridomi, mai=spacing)
    
    for(C in cases){
        for(f in rownames(facets)){
            v <- facets[[f,"raster"]]
            zz <- z[C,v,,]
            image(x, y, zz, zlim=zlims[[v]], col=cmaps[[v]],
                  xaxt='n', yaxt='n', ann=FALSE, ...)
            if(C==cases[1]) title(facets[[f,"title"]], line=1, cex=2, xpd=NA)
            if(C==cases[nc])            axis(1)
            if(f==rownames(facets)[nf]) axis(4)

            ## add map boundaries with optional halo
            mapargs <- list(add=TRUE, lwd=maplwd, col=mapcol,
                            hcol=ifelse(halo, hcolor, NA),
                            hlwd=ifelse(halo, hwidth, NA))
            mapply(halomap, dbs, regs, MoreArgs=mapargs)

            ## add arrows if vector variables defined
            if(!is.null(uv <- facets[[f,"vector"]])){
                if(arrowcol == "contrast") {
#                    amap <- bwcontrast(cmaps[[v]])[zz * 254/max(zz)+1]
                    amap <- bwcontrast(cmaps[[v]])[unitize(zz)*255]
#                    cat(C, f, range(unitize(zz)*255), table(bwcontrast(cmaps[[v]])), "\n")
                } else {
                    amap <- arrowcol
                }
                
                uu <- z[C,uv[1],,]
                vv <- z[C,uv[2],,]
                vectorfield(x, y, uu, vv, col=amap, fatten=fatten, length=alen)
            }

            ## add contours if contour variable defined
            if(!is.null(cv <- facets[[f,"contour"]])){
                ## aim for multiples of 50/100, not 20
                lev <- pretty(zlims[[cv]], u5.bias=5)
                contour(x, y, z[C,cv,,], add=TRUE, method="edge", lwd=conlwd,
                        levels = lev, col=concol, labcex=concex,
                        vfont=c("sans serif", "bold"))
            }
            if(!is.null(pointargs)){
                do.call(points, pointargs)
            }
        }
    }
    
    mtext(cases, side=2, outer=TRUE, line=0.5, at=(nc:1 - 1/2)/nc)
    mtext(main, side=3, line=2, outer=TRUE)
    
    ## colorbars at bottom of each column
    
    par(new=TRUE, omi=cbaromi, mai=c(0,1,0,1)*uniti + spacing,
        mfrow=c(1,nf), mfg=c(1,1))

    for(f in rownames(facets)){
        v <- facets[[f,"raster"]]
        cbmap <- cmaps[[v]]
        N <- length(cbmap)
        cbx <- do.call(seq, as.list(c(zlims[[v]], len=N)))
        cbz <- t(matrix(rep(1:N, each=3), nrow=3))
        
        image(x=cbx, z=cbz, col=cbmap, yaxt='n')
        mtext(gsub(" ", "\n", units[v]), side=2, las=2, cex=0.7, line=1/4)
    }
}



## Overloads maps::map().  Draws a map overlay with halo coloration
## for better contrast against image/contour maps both light and dark
## values.

## @param lwd:  base linewidth
## @param hlwd: linewidth for halo color
## @param col:  color for map boundaries
## @param hcol: color for halo around map boundaries.

halomap <- function(..., lwd=1, hlwd=2*lwd,
                    col="black", hcol="white", add=TRUE){
    halo = !is.na(hlwd) && !is.na(hcol)
    if(halo){ map(..., lwd=hlwd, col=hcol, add=add) }
    map(..., lwd=lwd, col=col, add=ifelse(halo, TRUE, add))
}


## Overplot arrows scaled to fit resolution of array

## @param x,y: 1-d vectors of x & y coordinates for data
## @param u,v: 2-d arrays of u & v components of vector field data

## @param fatten: how much to fatten arrows based on magnitude.  FALSE
## = 0 = no fattening, TRUE = 1 = double width for max, 2 = triple
## width for max, etc.

## @param length: length of the arrowhead in inches.

## @param lwd: line width for arrows

## @param ...: additional arguments to pass to arrows().

## return: invisibly returns the scaling factor, in case you want to
## use it in a legend or something.

vectorfield <- function(x, y, u, v, fatten=FALSE, length=0.015, lwd=par()$lwd, ...){

    nx <- length(x)
    ny <- length(y)

    ## note these look reversed if you print them b/c R is column-major
    x2d <- rep(x, ny)      |> matrix(nx, ny)
    y2d <- rep(y, each=nx) |> matrix(nx, ny)

    wscale <- mean(c(diff(x), diff(y))) / max(abs(range(c(u,v))))
    u2 <- u * wscale
    v2 <- v * wscale

    if(fatten){
        lwd <- lwd * (1 + fatten * unitize(sqrt(u^2 + v^2)))
    }
    
    arrows(x2d, y2d, x2d+u2, y2d+v2, lwd=lwd, length=length, ...)
    invisible(wscale)
}



## creates a colormap that is black where cmap is light and white
## where it's dark.

## TODO: figure out why all values of brewer.spectral cmap have V > 0.5

bwcontrast <- function(cmap){
    val <- (col2rgb(cmap) |> rgb2hsv())[3,]
    c("black","white")[1+(val < 0.5)]
}

## Inverts the cmap colors RGB-wise

invertcmap <- function(cmap){
    (255 - col2rgb(cmap)) |> t() |> rgb(max=255)
}


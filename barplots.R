source("names.R")
load("data/buckets.Rdata")
load("data/rdata/misc.Rdata")
load("plot/cmaps.Rdata")

outdir <- "plot/bucket"

mnum <- paste0("m", sprintf("%02d",1:12))
names(mnum) <- month.abb

gcms <- c("HadGEM","GFDL","MPI")  ## ordered by quality

test <- FALSE


skip <- function(x, y){ x[!(x %in% y)] }
methods <- levels(bstat$method) |> skip("gridMET")
nm <- length(methods)


## To get all 3 buckets on the same barchart, grouped and stacked,
## with different colormaps, we create 3 different barcharts and
## overlay them.

system(paste("mkdir -p", outdir))
bmap <- lapply(bucketmap, rev)

    
for(loc in "SGP-98-36"){
    #for(loc in unique(bstat$locname)){
    for(mon in 5){
        #    for(mon in 1:12) {
        
        month <- month.abb[mon]
        
        for(ptype in c("change", "grouped")){
            
            if(test){
                dev.new(height=8,width=9)
            } else {
                outname <- paste("bucket","bar", ptype, mnum[mon], loc, sep='.')
                outfile <- paste0(outdir, '/', outname, ".png")
                png(outfile, units="in", res=120, height=9, width=8)
            }

            reset <- par(no.readonly=TRUE)
            par(mfrow=c(1,length(gcms)), mar=c(5,3,0,0), oma=c(0,3,5,2), cex.axis=1.2)
            
            for(GCM in rev(gcms)){
                pbs <- subset(bstat, month==mon & locname==loc &
                                     gcm %in% c(GCM,"ERAI"))

                ## create NA array, fill in appropriate slots in 3 copies
                if(ptype == "change"){
                    pctarr <- array(dim=c(3, 1+(2+1)*nm),
                                    dimnames=list(bucket=rev(buckets), method=NULL))
                } else {
                    pctarr <- array(dim=c(3, 1+1+nm+1+nm),
                                    dimnames=list(bucket=rev(buckets), method=NULL))
                }
                
                inject <- function(x,a,i){ a[,i] <- x; return(a)}
                gsbar <- list(hist=pctarr, rcp85=pctarr, obs=pctarr)
                if(ptype == "change"){
                    slots <- list(hist=(1:nm)*3, rcp85=(1:nm)*3+1, obs=1)
                } else {
                    slots <- list(hist=(1:nm)+2, rcp85=(1:nm)+3+nm, obs=1)
                }

                ## reverse ordering, top to bottom
                slots <- lapply(slots, \(x){ncol(pctarr)+1-x})
                
                gsbar <- split(pbs[,c("scen","method","bucket","pct")],~scen) |>
                    lapply(reshape, direction="wide", drop="scen",
                           idvar="bucket", timevar="method") |>
                    lapply('[', -1) |>
                    lapply(as.matrix) |>
                    lapply(setname, buckets, "row") |>
                    mapply(FUN=inject, a=gsbar, i=slots, SIMPLIFY=FALSE)

                w <- 3
                if(ptype == "change"){
                    barwidth <- c(w, rep(c(1,w,w), nm)) |> rev()
                } else {
                    barwidth <- c(w, 1, rep(w, nm), 1, rep(w, nm)) |> rev()
                }
                
                ## blank plot w/ axes
                bpx <- barplot(pctarr, xlim=c(0,100), width=barwidth,
                               yaxt='n', horiz=TRUE)
                if(GCM == tail(gcms,1)){
                    if(ptype == "change"){
                        ax <- c((bpx[1:nm*3-2] + bpx[1:nm*3-1])/2, bpx[nm*3+1])
                        axis(side=2, las=1, labels=c(rev(methods),"obs"), at=ax)
                    } else {
                        axis(side=2, las=1, labels=c(methods,methods,"obs"),
                             at = unlist(sapply(slots, \(x){bpx[x]})))
                    }
                }
                mtext("%", side=2, line=0.3, at=-2, las=1)
                mtext(GCM, side=1, line=2.5)
                
                ## 3 overlaid barplots per scenario
                for(s in names(gsbar)){
                    plotme <- apply(gsbar[[s]], 2, rev)
                    barplot(plotme, col=bmap[[s]], axes=FALSE,
                            add=TRUE, width=barwidth, horiz=TRUE)
                }

                ## lines on top for obs & raw hist gcm
                ## use segments b/c for some reason abline extends too far
                bary <- tail(bpx,1) + diff(tail(bpx,2)) * 0.6 |> rep(4)
                obsx <- subset(pbs, scen=="obs")$pct |>
                                               rev() |>
                                               cumsum() |>
                                               head(-1)
                rawx <- subset(pbs, method=="raw" & scen=="hist")$pct |>
                                                                rev() |>
                                                                cumsum() |>
                                                                head(-1)
                
                segments(c(obsx, rawx), rep(0,4), c(obsx, rawx), bary,
                         col='red', lty=c("solid","solid","dotted","dotted"))
            }

            mtext(paste(month, "wet/dry days, ", loc), side=3, outer=TRUE,
                  las=1, cex=1.5, line=0.5)
            
            
            ## little 3x3 legend
            
            par(new=TRUE, fig=c(85,100,92,100)/100, mar=c(1,1,0,0),
                oma=c(0,0,1,2), las=1)
            legarr <-array(1:9, dim=c(3,3),
                           dimnames=list(names(bucketmap), rev(buckets)))
            image(1:3, 1:3, legarr, col=unlist(rev(bmap)),
                  axes=FALSE, ann=FALSE)
            mtext(side=1, rev(buckets), at=1:3, cex=2/3)
            mtext(side=2, rev(names(bmap)), at=1:3, adj=1, line=0.5, cex=2/3)
            par(reset)
            
            if(!test){
                dev.off()
            }
        }
    }

    ## annual cycle plots        
        
    panels <- c(3,3)
    size <- list(width=8, height=5)
    
    for(GCM in gcms){
        for(s in scen[-1]){
            ann <- subset(bstat, locname==loc &
                                 gcm %in% c("ERAI",GCM) &
                                 scen %in% c(s, "obs") &
                                 !(method == "dummy")) |>
                split(~ method + gcm + scen, drop=TRUE) |>
                lapply(subset, subset=TRUE,
                       select=c("bucket","month","pct")) |>
                lapply(reshape, dir="wide",
                       idvar="bucket",
                       timevar="month") |>
                lapply(tail, n=c(NA,-1)) |>
                lapply(as.matrix) |>
                lapply(setname, nm=list(buckets, month.abb), ntype="all")

            if(test){
                do.call(dev.new, size)
            } else {
                outname <- paste("cycle", "bar", s, GCM, loc, sep='.')
                outfile <- paste0(outdir, '/', outname, ".png")
                do.call(png, c(list(file=outfile, units="in", res=120), size))
            }
            
            par(mfcol=panels, las=2, oma=c(3,0,3,0),
                mar=c(3,2.5,2,0.5), mgp=c(3,0.5,0))
            for(i in names(ann)){
                wetdry <- apply(ann[[i]], 2, rev)
                cmap <- bmap[sapply(scen, grepl, i)] |> unlist()
                barplot(wetdry, main=i, col=cmap)
            }
            mtext(paste(GCM,"wet/dry days,", loc), outer=TRUE, side=3, las=1)
            ## outer margin legend
            par(mfrow=c(1,1), oma=rep(0,4), mar=rep(0,4), new=TRUE)
            plot(0:1, 0:1, type='n', axes=FALSE, ann=FALSE)
            legend("bottom", rev(buckets), fill=bmap$hist, horiz=TRUE)
            if(!test){
                dev.off()
            }
        }
    }
}


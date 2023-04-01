source("names.R")
load("data/buckets.Rdata")
load("data/rdata/misc.Rdata")
load("plot/cmaps.Rdata")

outdir <- "plot/bucket"
system(paste("mkdir -p", outdir))

mnum <- paste0("m", sprintf("%02d",1:12))
names(mnum) <- month.abb

methods <- head(levels(bstat$method), -1)
nm <- length(methods)

gcms <- c("HadGEM","MPI","GFDL")  ## ordered by ECS


## To get all 3 buckets on the same barchart, grouped and stacked,
## with different colormaps, we create 3 different barcharts and
## overlay them.


for(loc in "SGP-98-36"){
    #for(loc in unique(bstat$locname)){
    for(mon in 5){
        #    for(mon in 1:12) {

        month <- month.abb[mon]

        for(ptype in c("change", "grouped")){

            outname <- paste("bucket","bar", ptype, mnum[mon], loc, sep='.')
            outfile <- paste0(outdir, '/', outname, ".png")
            png(outfile, units="in", res=120, height=9, width=8)
            
#            dev.new(height=9,width=8)

            par(mar=c(1,4,2,0), mfrow=c(length(gcms),1), oma=c(5,0,5,2),
                cex.axis=1.2)

            for(GCM in gcms){
                pbs <- subset(bstat, month==mon & locname==loc &
                                     gcm %in% c(GCM,"ERAI"))

                ## create NA array, fill in appropriate slots in 3 copies
                pctarr <- array(dim=c(3, 1+3*nm),
                                dimnames=list(bucket=buckets, method=NULL))

                inject <- function(x,a,i){ a[,i] <- x; return(a)}
                gsbar <- list(hist=pctarr, rcp85=pctarr, obs=pctarr)
                if(ptype == "change"){
                    slots <- list(hist=(1:nm)*3, rcp85=(1:nm)*3+1, obs=1)
                } else {
                    slots <- list(hist=(1:nm)+2, rcp85=(1:nm)+3+nm, obs=1)
                }

                gsbar <- split(pbs[,c("scen","method","bucket","pct")],~scen) |>
                    lapply(reshape, direction="wide", drop="scen",
                           idvar="bucket", timevar="method") |>
                    lapply('[', -1) |>
                    lapply(as.matrix) |>
                    lapply(setname, buckets, "row") |>
                    mapply(FUN=inject, a=gsbar, i=slots, SIMPLIFY=FALSE)

                w <- 3
                if(ptype == "change"){
                    barwidth <- c(w, rep(c(1,w,w), nm))
                } else {
                    barwidth <- c(w, 1, rep(w, nm), 1, rep(w, nm), rep(0, nm-3))
                }
                
                ## blank plot w/ axes
                bpx <- barplot(pctarr, ylim=c(0,100), width=barwidth, xaxt='n')
                if(GCM == tail(gcms,1)){
                    if(ptype == "change"){
                        ax <- c(bpx[1], (bpx[1:nm*3] + bpx[1:nm*3 + 1])/2)
                        axis(side=1, las=3, labels=c("obs",methods), at=ax)
                    } else {
                        axis(side=1, las=3, labels=c(methods,methods,"obs"),
                             at = unlist(sapply(slots, \(x){bpx[x]})))
                    }
                }
                mtext("%", side=2, line=2)
                mtext(GCM, side=4, line=0)
                
                ## 3 overlaid barplots per scenario
                for(s in names(gsbar)){
                    barplot(gsbar[[s]], col=bucketmap[[s]], axes=FALSE,
                            add=TRUE, width=barwidth)
                }

                ## lines on top for obs & raw hist gcm
                ## use segments b/c for some reason abline extends too far right
                barx <- tail(bpx,1) + diff(tail(bpx,2)) * 0.6 |> rep(4)
                obsy <- subset(pbs, scen=="obs")$pct   |> cumsum() |> head(-1)
                rawy <- subset(pbs, method=="raw" & scen=="hist")$pct |>
                                                                cumsum() |>
                                                                head(-1)

                segments(rep(0,4), c(obsy, rawy), barx, c(obsy,rawy),
                         col='red', lty=c("solid","solid","dotted","dotted"))
            }

            mtext(paste(month, "wet/dry days, ", loc), side=3, outer=TRUE,
                  las=1, cex=1.5, line=0.5)

 
            ## little 3x3 legend
            
            reset <- par(new=TRUE, fig=c(85,100,92,100)/100, mar=c(1,1,0,0),
                         oma=c(0,0,1,2), las=1)
            legarr <-array(1:9, dim=c(3,3),
                           dimnames=list(names(bucketmap), buckets))
            image(1:3, 1:3, t(legarr), col=unlist(bucketmap),
                  axes=FALSE, ann=FALSE)
            mtext(side=2, buckets, at=1:3, adj=1, line=0.5, cex=2/3)
            mtext(side=1, names(bucketmap), at=1:3, cex=2/3)
            par(reset)
            
            dev.off()
        }
    }
}

 

source("names.R")
load("data/buckets.Rdata")
load("data/misc.Rdata")
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


#for(loc in unique(bstat$locname)){
#    for(mon in 1:12) {

loc = "SGP-98-38"
mon = 5

month <- month.abb[mon]


#outname <- paste("bucket", "stackbar", x, y, mnum[m], sep='-')
#outfile <- paste0(outdir, '/', outname, ".png")
#png(outfile, units="in", res=120, height=9, width=8)


dev.new(height=9,width=8)


par(mar=c(1,4,2,0), mfrow=c(length(gcms),1), oma=c(5,0,5,2), cex.axis=1.2)


for(GCM in gcms){
pbs <- subset(bstat, month==mon & locname==loc & gcm %in% c(GCM,"ERAI"))

## create NA array, fill in appropriate slots in 3 copies
pctarr <- array(dim=c(3, 1+3*nm), dimnames=list(bucket=buckets, method=NULL))

inject <- function(x,a,i){ a[,i] <- x; return(a)}
gsbar <- list(hist=pctarr, rcp85=pctarr, obs=pctarr)
slots <- list(hist=(1:nm)*3, rcp85=(1:nm)*3+1, obs=1)

gsbar <- split(pbs[,c("scen","method","bucket","pct")], ~scen) |>
    lapply(reshape, direction="wide", drop="scen",
           idvar="bucket", timevar="method") |>
    lapply('[', -1) |>
    lapply(as.matrix) |>
    lapply(setname, buckets, "row") |>
    mapply(FUN=inject, a=gsbar, i=slots, SIMPLIFY=FALSE)


## labels for bars (with blanks to get spacing right)
mnm <- rep("", ncol(pctarr))
mnm[1] <- "obs"
mnm[3*1:nm] <- methods

barwidth <- c(3,1,3)

bpx <- barplot(pctarr, ylim=c(0,100), width=barwidth, xaxt='n')
if(GCM == tail(gcms,1)){
    ax <- c(bpx[1], (bpx[1:nm*3] + bpx[1:nm*3 + 1])/2)
    axis(side=1, las=3, labels=c("obs",methods), at=ax)
}
mtext("%", side=2, line=2)
mtext(GCM, side=4, line=0)

## use segments b/c for some reason abline extends too far right
barx <- tail(bpx,1) + diff(tail(bpx,2)) * 0.6 |> rep(4)
obsy <- subset(pbs, scen=="obs")$pct   |> cumsum() |> head(-1)
rawy <- subset(pbs, method=="raw" & scen=="hist")$pct |> cumsum() |> head(-1)

segments(rep(0,4), c(obsy, rawy), barx, c(obsy,rawy),
         col="gray", lty=c("solid","solid","dotted","dotted"), lwd=2)


for(s in names(gsbar)){
    barplot(gsbar[[s]], col=bucketmap[[s]], axes=FALSE, add=TRUE, width=barwidth)
}

}

mtext(paste(month, "wet/dry days, ", loc), side=3, outer=TRUE, las=1, cex=1.5, line=0.5)

 
 ## little 3x3 legend
 
reset <- par(new=TRUE, fig=c(85,100,92,100)/100, mar=c(1,1,0,0), oma=c(0,0,1,2))
legarr <-array(1:9, dim=c(3,3), dimnames=list(names(bucketmap),buckets))
image(1:3, 1:3, legarr, col=unlist(bucketmap), axes=FALSE, ann=FALSE)
mtext(side=1, buckets, at=1:3, las=1, cex=2/3)
mtext(side=2, names(gsbar), at=1:3, las=2, cex=2/3, adj=1, line=0.5)
par(reset)

#dev.off()
#}
#}
#}
 

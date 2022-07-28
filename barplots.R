load("data/buckets.Rdata")
load("data/misc.Rdata")
load("plot/cmaps.Rdata")

## barplots

x = "x098"
y = "y38"
m = "May"

## To get all 3 datasets on the same barchart, grouped and stacked,
## with different colormaps, we create 3 different barcharts and
## overlay them.

nm <- length(methods)

## create NA array, fill in appropriate slots in 3 copies
pctarr <- array(dim=c(3, 1+3*nm), dimnames=list(bucket=buckets, method=NULL))

gsbar <- list(obs=pctarr, hist=pctarr, rcp85=pctarr)

gsbar$obs[,1] <- bstat$pct$obs[,1,x,y,m]
gsbar$hist[,(1:nm)*3] <- bstat$pct$hist[,,x,y,m]
gsbar$rcp85[,(1:nm)*3+1] <- bstat$pct$rcp85[,,x,y,m]

## labels for bars (with blanks to get spacing right)
mnm <- rep("", ncol(pctarr))
mnm[1] <- "obs"
mnm[3*1:nm] <- methods

#dev.new(height=5,width=8)
system("mkdir -p plot/bucket")
png("plot/bucket/may-stack-bar.png", units="in", res=120, height=5,width=8)
par(mar=c(5,4,4,2))

barwidth <- c(3,1,3)

barplot(pctarr, ylim=c(0,100), width=barwidth,
        main=paste(m,"precip,",x,y), names.arg=mnm, las=2)
mtext("%", side=2, line=2)
abline(h=cumsum( gsbar$obs[,1])*100, col="darkgray", lty=3)
abline(h=cumsum(gsbar$hist[,3])*100, col="darkgray", lty=2)

for(p in periods){
    barplot(gsbar[[p]]*100, col=bucketmap[[p]], axes=FALSE, add=TRUE, width=barwidth)
}

## little 3x3 legend

reset <- par(new=TRUE, fig=c(80,95,85,98)/100, mar=c(1,1,0,0))
legarr <-array(1:9, dim=c(3,3), dimnames=list(names(gsbar),buckets))
image(1:3, 1:3, legarr, col=unlist(bucketmap), axes=FALSE, ann=FALSE)
mtext(side=1, buckets, at=1:3, las=1, cex=2/3)
mtext(side=2, names(gsbar), at=1:3, las=2, cex=2/3, adj=1, line=0.5)
par(reset)

dev.off()

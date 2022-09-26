load("data/buckets.Rdata")
load("data/misc.Rdata")
load("data/prec.Rdata")

maydata <- prec$hist[with(prec$hist, month==5 & px=="x098" & py=="y36"),]
mayobs <- prec$obs[with(prec$obs, month==5 & px=="x098" & py=="y36"),]


dev.new(width=10,height=5)

par(mfcol=c(4,8), mar=c(2.5,2.5,1,1), oma=c(0,0,4,0))

for(m in methods[-1]){
    distplots(maydata[,"raw"]^0.25, maydata[,m]^0.25)
}
mtext(methods[-1], side=3, outer=TRUE, line=0, at=1:8/8-1/16)
mtext("MPI precip^(1/4) distributions vs raw", side=3, outer=TRUE, cex=1.5, line=1.5)


## Ah, want to drop zeros...

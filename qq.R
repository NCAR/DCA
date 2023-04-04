library(pals)

source("util.R")
source("names.R")

load("data/prec.SGP.all.Rdata")
load("data/buckets.Rdata")

## TRUE = plot onscreen, FALSE = plot to file
test = FALSE

plotdir <- "plot/qq"
system(paste("mkdir -p", plotdir))

## looping goes here
loc <- "SGP-98-36"
mon <- 5
mnum <- paste0("m",sprintf("%02d",mon))
mname <- month.abb[mon]
plotbase <- paste(mnum, loc, "png", sep='.') 

modmethods <- levels(bstat$method) |> head(-2)  # drop dummy, gridMET

obs <- subset(bstat, scen == "obs" & month == mon & locname == loc)
mod <- subset(bstat, scen != "obs" & month == mon & locname == loc &
                     method %in% modmethods) |> droplevels()

gcms <- levels(mod$gcm)
obins <- cumsum(obs$pct[order(obs$bucket, buckets)]) |> setname(buckets)
omids <- mids(c(0, obins)) |> setname(buckets)

bpal <- brewer.set1(8) |>
    setname(c("red","blu","grn","pur","orn","yel","brn","pnk")) |>
    c(blk="#000000", gry="#888888")

cmap <- c(raw='blk', RegCM4='grn', WRF='red', CNN='pur', LOCA='blu',
          SDSM='orn', qdm='pnk', simple='brn', dummy='yel', obs='gry')

mpal <- bpal[cmap] |> setname(names(cmap))

mpch <- c(raw=1, RegCM4=2, WRF=6, CNN=0, LOCA=5, SDSM=8, qdm=3,
          simple=4, dummy=20)

## methods that have a value for the 31st of the month for HadGEM
## (which we need to drop, since not everything has it)
has31st <- c("WRF","CNN","LOCA")

pr <- subset(prec, scen == "hist" & month == mon & locname == loc) |>
    subset(!(gcm=="HadGEM" & method %in% has31st & day==31))

## omit raw & gridMET
downmethods <- levels(pr$method) |> head(-1) |> tail(-1)

xyr <- c(trace, max(pr$prec))



## Q-Q plots

qmethods <- tail(modmethods, -1)


for(GCM in gcms){

    if(test){
        dev.new()
    } else {
        png(file=paste0(plotdir, '/qq.', GCM, '.', plotbase),
            width=7, height=7, units='in', res=120)
    }
                
    p <- subset(pr, gcm==GCM)
    praw <- subset(p, method=="raw")$prec
    
    plot(NA, log='xy', xlim=xyr, ylim=xyr,
         xlab="raw precip (mm)", ylab="method precip (mm)",
         main=paste(GCM, mname, loc, "Q-Q plot"))
    
    abline(0,1)
    abline(h=theta, v=theta, col='gray', lty=1)
    
    for(m in rev(qmethods)){
        pmod <- subset(p, method==m)$prec
        points(sort(praw), sort(pmod), col=mpal[m], pch=mpch[m])
    }

    legend("topleft", ncol=2,
           c(qmethods,"3 mm"),
           pch=c(mpch[qmethods], NA),
           col=c(mpal[qmethods],"gray"),
           lty=c(rep(NA,length(qmethods)), 1))
    if(!test){
        dev.off()
    }
}




##################
## confusion plots

prconf <- subset(prec, scen != "obs" & month == mon & locname == loc) |>
    subset(!(gcm=="HadGEM" & method %in% has31st & day==31)) |>
    subset(!(scen=="rcp85" & year < 2076)) |>
    droplevels()

prconf$bucket <- bucketize(prconf$prec)


cbase <- subset(bstat, scen != "obs" & month == mon & locname == loc,
                select=c("gcm","method","scen","bucket")) |>
    droplevels()

confusion <- cbind(rbind(cbase, cbase, cbase),
                   gbucket=factor(rep(buckets, each=nrow(cbase))),
                   pct=NA)
cmeth <- levels(confusion$method)

cs <- split(confusion, ~ gcm + scen + method)

for(g in gcms){
    for(s in levels(confusion$scen)){
        praw <- subset(prconf, gcm==g & scen==s & method=="raw")
        for(m in cmeth){ 
            psub <- subset(prconf, gcm==g & scen==s & method==m)
            cmat <- table(raw=praw$bucket, mod=psub$bucket)
            id <- paste(g, s, m, sep='.')
            cs[[id]]$pct <- c(t(cmat)) / sum(cmat)
        }
    }
}

conf <- do.call(rbind, cs) |> setname(NULL, "row")

## barplots in a 3x3 matrix


## give bars for future 50% alpha
cpal <- mpal[cmeth] |> rep(each=2) |> mapply(FUN=adjustcolor, c(1,0.5))

nm <- length(cmeth)

yr <- c(0, max(conf$pct)) * 100

for(GCM in gcms){

    if(test) {
        dev.new()
    } else {
        png(file=paste0(plotdir, '/conf.',GCM,'.', plotbase),
            width=7, height=7, units='in', res=120)
    }
    par(mfrow=c(3,3), mar=c(1,3,1,1), oma=c(4,2,2,0))
    
    for(gb in buckets){
        for(b in rev(buckets)){
            csub <- subset(conf, gcm==GCM & gbucket==gb & bucket==b)
            yy <- (csub$pct * 100) |>
                matrix(nrow=2) |>
                setname(levels(csub$method), "col")
            barplot(yy, ylim=yr, beside=TRUE, axisnames=FALSE, col=cpal)
            barplot(yy, ylim=yr, beside=TRUE, axisnames=FALSE, col="gray",
                    density=c(0, 20), add=TRUE)
        }
    }
    mpos <- c(1,3,5)/6
    mtext(outer=TRUE, side=1, paste("GCM",buckets), at=mpos)
    mtext(outer=TRUE, side=2, paste("method",buckets,"(% days)"), at=mpos)
    mtext(outer=TRUE, side=3, paste(GCM, mname, loc,
                                    "downscaling confusion matrix"))

    par(mfrow=c(1,1), oma=c(0,0,0,0), mar=c(0,0,0,0), new=TRUE)
    plot(NA, xaxt='n', yaxt='n', xlim=c(0,1), ylim=c(0,1), new=TRUE)
    legend("bottom", c(cmeth, "rcp85"), fill=c(mpal[cmeth], "gray"),
           density=c(rep(NA, nm), 20), ncol=length(cmeth)+1, cex=0.6)

    if(!test){
        dev.off()
    }
}

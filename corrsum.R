library(abind)
library(maps)

source("names.R")
source("util.R")
source("~/climod/R/renest.R")

load("data/rdata/misc.Rdata")
load("data/rdata/ua.meta.Rdata")

clean <- function(x){x[abs(x)>1e20]<-NA; return(x)}

mag <- function(x,y){sqrt(x^2+y^2)}

test <- FALSE
#test <- TRUE

infiles <- dir("data/anom", rec=TRUE, full=TRUE)

## drop loc & month as constant in this analysis
locx <- -98
locy <- 36

abits <- strsplit(infiles, '/')  |>
    sapply(tail, 1) |>
    strsplit('.', fixed=TRUE) |>
    sapply(head,3) |>
    t()

anames <- apply(abits, 1, paste, collapse=".")

ameta <- as.data.frame(abits) |>
    mapply(FUN=factor, SIMPLIFY=FALSE, list(methods, scen, gcms)) |>
    as.data.frame() |>
    setname(c("method","scen","gcm"), "col") |>
    setname(anames, "row")

## re-sort for analysis
ameta <- ameta[with(ameta, order(scen, gcm, method)),]

## read in & clean anomaly data
anom <- lapply(infiles, listload) |>   ##listload from util.R
    lapply('[[',1) |>
    setname(anames) |>
    rapply(clean, how="replace") |>
    renest()


## subset to analysis box

xlim <- c(-113, -85)
ylim <- c(28, 44)

dlon <- lon[lon %within% xlim]
dlat <- lat[lat %within% ylim]

delta <- renest(anom$delta)$wet |> abind(along=0)
delta <- delta[,, lon %within% xlim, lat %within% ylim]


## calculate S850 & wind directions
delta2 <- abind(along=2, delta,
                S850 =  mag(delta[,"V850",,], delta[,"U850",,]),
                TH850=atan2(delta[,"V850",,], delta[,"U850",,]),
                TH250=atan2(delta[,"V250",,], delta[,"U250",,])
                )
dvars <- dimnames(delta2)[[2]]

## uniform missing-data mask - only need to apply to obs

var850 <- grep(850, dvars, val=TRUE)
var700 <- grep(700, dvars, val=TRUE)

m850 <- apply(delta2[,var850,,], c(3,4), sum) |> is.finite()
m700 <- apply(delta2[,var700,,], c(3,4), sum) |> is.finite()

obs <- delta2[1,,,]
mod <- delta2[-1,,,]

for(v in var850){ obs[v,,][!m850] <- NA }
for(v in var700){ obs[v,,][!m700] <- NA }


## interdecile range used to scale MAE values
IDR <- apply(obs, 1, quantile, na.rm=TRUE, probs=c(0.1, 0.9)) |>
    apply(2, diff)


####################
## calculate metrics
modnames <- dimnames(mod)[[1]]

## we don't use all of these, but it's simpler to have/calc all
metvarnames <- c(U850="850-mb U-wind",
                 V850="850-mb V-wind",
                 Q850="850-mb moisture",
                 T700="700-mb temperature",
                 Z700="700-mb geopotential",
                 Z500="500-mb geopotential",
                 U250="250-mb U-wind",
                 V250="250-mb V-wind",
                 A850="850-mb moisture flux",
                 S250="250-mb wind speed",
                 S850="850-mb wind speed",
                 UV850="850-mb winds",
                 UV250="250-mb winds")
metvars <- names(metvarnames)

metrics <- merge(ameta[-1,], list(vars=metvars))
rownames(metrics) <- apply(metrics, 1, paste, collapse='.')

metrics$cor <- NA
metrics$mae <- NA
metrics$err <- NA

for(m in modnames){
    for(v in head(metvars, -2)){
        id <- paste(m, v, sep='.')
        metrics[id,"cor"] <- cor(c(obs[v,,]), c(mod[m,v,,]), use="pair")
        metrics[id,"mae"] <- mean(abs(obs[v,,] - mod[m,v,,]), na.rm=TRUE)
        metrics[id,"err"] <- metrics[id,"mae"] / IDR[v]
    }
    for(height in c(850, 250)){
        p <- \(x){paste0(x,height)}
        id <- paste(m, p("UV"), sep='.')
        metrics[id,"cor"] <- mean(cos(obs[p("TH"),,] - mod[m,p("TH"),,]), na.rm=TRUE)
        metrics[id,"mae"] <- mean(mag(obs[p("U"),,]  - mod[m,p("U"),,],
                                      obs[p("V"),,]  - mod[m,p("V"),,]), na.rm=TRUE)
        metrics[id,"err"] <- metrics[id,"mae"] / IDR[p("S")]
    }
}



## convert factors to character (for plotting) & round numbers (for
## printing)

numcols <- c("cor","mae", "err")
metrics[numcols] <- lapply(metrics[numcols], round, digits=3)
factcols <- c("method","scen","gcm")
metrics[factcols] <- lapply(metrics[factcols], as.character)    
    
write.csv(metrics, file="plot/corr.wet.delta.csv", row.names=FALSE)

########
### plot


gcol <- c(HadGEM=2, GFDL=3, MPI=4) # red, green, blue
fade <- 0.3
lcol <- adjustcolor(gcol, fade)
names(lcol) <- names(gcol)
lleg <- adjustcolor("black", fade)

rsym <- c(raw=19,       # big dot #raw=20,       # small dot
          RegCM4=2,     # up triangle
          WRF=6,        # down triangle
          CNN=13,       # crossed circle
          LOCA=8,       # *
          SDSM=3,       # +
          qdm=126,      # ~
          simple=124,   # |
          dummy=1)      # o


if(test){
    dev.new()
} else {
    png(file="plot/corr-mae.png", width=6, height=6, units="in", res=180)
}

par(mfrow=c(3,3), mar=c(3.1, 3.1, 2, 1), mgp=c(1.5,0.5,0))

for (v in c("A850",  "T700", "UV250",
            "UV850", "Z700", "legend",
            "Q850",  "Z500", "map")){
    if(v %in% metvars){ 
        m <- subset(metrics, vars==v & scen=="hist")
        plot(m$cor, m$err, xlim=c(-1,1), ylim=c(0,1.5),
             xlab="correlation", ylab="scaled error",
             main=metvarnames[v], cex=1.3,
             pch=rsym[m$method], col=gcol[m$gcm])
                
        for(g in names(gcol)){
            mm <- subset(m, gcm==g & method != "dummy")
            segments(mm[1,"cor"], mm[1,"err"], mm[-1,"cor"], mm[-1,"err"],
                     col=lcol[g], lty=c(1,1,2,2,2,3,3))
        }
    } else {
        if(v == "legend"){
        plot(c(0,1), c(0,1), pch=NA, ann=FALSE, axes=FALSE)
        legend("top", names(rsym), pch=rsym, ncol=2, cex=0.9)
        legend("bottomleft", names(gcol), fill=gcol, cex=0.8)
        legend("bottomright", lty=1:3, col=lleg, cex=0.8,
               c("dynamic", "spatial","point"))
        }
        if(v == "map"){
            plot(NA, xlim=xlim, ylim=ylim, main="analysis region",
                 xlab="lon", ylab="lat")
            map("state", ".", add=TRUE)
            points(locx, locy, pch=23, col="black", bg="red")
        }
    }
}
if(!test){ dev.off() }


##############
## credibility 

## Since we've managed to get everything on the same scale, we can
## just do Manhattan distance from the lower-right corner.

metrics$cred <- (metrics$cor - metrics$err + 2)/3

credvars <- c("A850","T700","UV250","UV850","Q850","Z700","Z500")

cred <- xtabs(cred ~ gcm + method,
              data=subset(metrics, scen=="hist" & vars %in% credvars))

## reorder for plotting, divide by num methods
cred <- cred[names(gcol), methods[-1]] / 10

if(test){
dev.new(width=4, height=4)
} else {
    png(file="plot/credibility.png", width=4, height=4, units="in", res=180)
}
par(mar=c(4.5, 4.5, 2, 2))

plot(t(unclass(cred)), rep(9:1, 3), pch=rep(rsym, 3),
     col=rep(gcol, each=9), xlim=c(0, max(cred)), cex=1.5,
     xlab="relative credibility score", ylab='', yaxt='n')
axis(side=2, at=9:1, labels=methods[-1], las=2)

abline(v=cred[,"raw"], col=lcol)
abline(v=cred[,"dummy"], col=lcol, lty=2)

legend("topleft", names(gcol), fill=gcol, cex=4/5) 

if(!test){dev.off()}

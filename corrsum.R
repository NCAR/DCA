library(abind)

source("names.R")
source("util.R")
source("~/climod/R/renest.R")

load("data/rdata/misc.Rdata")
load("data/rdata/ua.meta.Rdata")

clean <- function(x){x[abs(x)>1e20]<-NA; return(x)}

infiles <- dir("data/anom", rec=TRUE, full=TRUE)

innames <- strsplit(infiles, '/')  |>
    sapply(tail, 1) |>
    gsub(pat=".Rdata", rep='')

ameta <- strsplit(innames, '.', fixed=TRUE) |>
    do.call(what=rbind) |>
    data.frame() |>
    setname(c("method","scen","gcm","loc","month")) |>
    setname(innames, "row")

## factors ordered for sorting
ameta$scen <- factor(ameta$scen, levels=scen)
ameta$gcm   <- gsub("2","", ameta$gcm) |> factor(levels=gcms)
ameta$method <- factor(ameta$method, levels=methods)

ameta <- ameta[with(ameta, order(scen, gcm, method)),]


anom <- lapply(infiles, listload) |>   ##listload from util.R
    lapply('[[',1) |>
    setname(innames) |>
    rapply(clean, how="replace") |>
    renest()


## subset to analysis box

xlim <- c(-113, -85)
ylim <- c(28, 44)


delta <- renest(anom$delta)$wet |> abind(along=0)
delta <- delta[,, lon %within% xlim, lat %within% ylim]

## uniform missing-data mask - only need to apply to obs

var850 <- grep(850, vars, val=TRUE)
var700 <- grep(700, vars, val=TRUE)

m850 <- apply(delta[,var850,,], c(3,4), sum) |> is.finite()
m700 <- apply(delta[,var700,,], c(3,4), sum) |> is.finite()

obs <- delta[1,,,]
mod <- delta[-1,,,]

for(v in var850){ obs[v,,][!m850] <- NA }
for(v in var700){ obs[v,,][!m700] <- NA }

mn <- dimnames(mod)[[1]]

metrics <- merge(ameta, as.data.frame(vars))
rownames(metrics) <- apply(metrics, 1, paste, collapse='.')

metrics$cor <- NA
metrics$mae <- NA

for(m in mn){
    for(v in vars){
        id <- paste(m, v, sep='.')
        metrics[id,"cor"] <- cor(c(obs[v,,]), c(mod[m,v,,]), use="pair")
        metrics[id,"mae"] <- mean(abs(obs[v,,] - mod[m,v,,]), na.rm=TRUE)
    }
}


numcols <- c("cor","mae")
metrics[numcols] <- lapply(metrics[numcols], round, digits=3)
factcols <- c("method","scen","gcm")
metrics[factcols] <- lapply(metrics[factcols], as.character)    
    
write.csv(metrics, file="plot/corr.wet.delta.csv", row.names=FALSE)


###

gcol <- c(HadGEM=2, MPI=4, GFDL=3) # red, blue, green

rsym <- c(raw=20,       # small dot
          RegCM4=2,     # up triangle
          WRF=6,        # down triangle
          CNN=13,       # crossed circle
          LOCA=8,       # *
          SDSM=3,       # +
          qdm=39,       # '
          simple=45,    # -
          dummy=1)      # o

#dev.new(width=10, height=10)
png(file="plot/corr-mae.png", width=10, height=10, units="in", res=120)
par(mfcol=c(3,3), mar=c(3.1, 3.1, 2, 1), mgp=c(1.5,0.5,0))

for (v in c("A850", "U850", "V850",
            "T700", "legend", "Z700",
            "S250", "U250", "V250")){
    if(v == "legend"){
        plot(c(0,1), c(0,1), pch=NA, ann=FALSE, axes=FALSE)
        legend("top", names(rsym), pch=rsym, ncol=2, cex=1.3)
        legend("bottom", names(gcol), fill=gcol)
    } else {
        m <- subset(metrics, vars==v & scen=="hist")
        plot(m$cor, m$mae, #xlim=c(-1,1), ylim=c(0,max(m$mae)),
         main=v, xlab="correlation", ylab="MAE",
         pch=rsym[m$method], col=gcol[m$gcm], cex=1.3)
    }
}
dev.off()

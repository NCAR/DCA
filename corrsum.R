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

dlon <- lon[lon %within% xlim]
dlat <- lat[lat %within% ylim]

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

#gcol <- c(HadGEM=2, MPI=4, GFDL=3) # red, blue, green
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

# png(file="plot/corr-mae.png", width=6, height=6, units="in", res=180)
# par(mfcol=c(3,3), mar=c(3.1, 3.1, 2, 1), mgp=c(1.5,0.5,0))
# 
# for (v in c("A850", "U850", "V850",
#             "T700", "legend", "Z700",
#             "S250", "U250", "V250")){
#     if(v == "legend"){
#         plot(c(0,1), c(0,1), pch=NA, ann=FALSE, axes=FALSE)
#         legend("top", names(rsym), pch=rsym, ncol=2, cex=0.9)
#         legend("bottomleft", names(gcol), fill=gcol, cex=0.8)
#         legend("bottomright", lty=1:3, col=lleg, cex=0.8,
#                c("dynamic", "spatial","point"))
#     } else {
#         m <- subset(metrics, vars==v & scen=="hist")
#         plot(m$cor, m$mae, #xlim=c(-1,1), ylim=c(0,max(m$mae)),
#          main=v, xlab="correlation", ylab="MAE",
#          pch=rsym[m$method], col=gcol[m$gcm], cex=1.3)
# 
#         for(g in names(gcol)){
#             mm <- subset(m, gcm==g & method != "dummy")
#             segments(mm[1,"cor"], mm[1,"mae"], mm[-1,"cor"], mm[-1,"mae"],
#                      col=lcol[g], lty=c(1,1,2,2,2,3,3))
#         }
#     }
# }
# dev.off()

# 
# ## average correlations
# ## convert MAE to rank product statistic
# 
# calcrps <- function(df, colname){
#     df$rps <- 1/rank(df[[colname]])
#     return(df)
# }
# 
# ## geometric mean
# gmean <- function(x){exp(mean(log(x)))}
# 
# 
# ## apply to only some columns
# capply <- function(X, MARGIN, FUN, COLS, ...){
#     X[,COLS] <- apply(X[,COLS], MARGIN, FUN, ...)
#     return(X)
# }
#     
# met2 <- subset(metrics, scen=="hist",
#                select=c("method","gcm", "vars","cor","mae")) |>
#     split(~ vars + gcm) |>
#     lapply(calcrps, "mae") |>
#     do.call(what=rbind)
# 
# 
# mcor <- split(met2, ~ method) |> lapply('[[',"cor") |> sapply(mean)
# mrps <- split(met2, ~ method) |> lapply('[[',"rps") |> sapply(gmean)
# 
# m3 <- sqrt(mcor*mrps) |> sort(dec=TRUE)
# #? m3 <- (mcor+mrps)/2 |> sort(dec=TRUE)
# print(format(m3, digits=2))
# barplot(m3, las=2, main="metric")
# 
# mgcor <- split(met2, ~ method + gcm) |> lapply('[[',"cor") |> sapply(mean)
# mgrps <- split(met2, ~ method + gcm) |> lapply('[[',"rps") |> sapply(gmean)
# mg3 <- (mgcor+mgrps)/2
# 
# mg33 <- c(sort(mg3[1:9+9]), NA, sort(mg3[1:9]), NA, sort(mg3[1:9+18])) |> rev()
# 
# par(las=2, mar=c(8,4,4,1))
# barplot(mg33, main="metric", col=c(rep(4,9), NA, rep(3,9), NA, rep(2,9)))
# 
# print(format(mg33, digits=3))

## This doesn't really seem right.

## I think the rank product statistic isn't working & we need to
## normalize by variable.  Maybe also handle (vector) winds
## differently, but first let's just try normalizing.

## Check distributions

## (Skip Z500, since it's not included in plots and is highly
## correlated with Z700)

mvars <- vars[!vars %in% "Z500"]

omean <- apply(obs, 1, mean, na.rm=TRUE)
osd   <- apply(obs, 1, sd, na.rm=TRUE)

# dev.new()
# par(mfrow=c(3,3))
# for(v in mvars){
#     plot(density(obs[v,,], na.rm=TRUE), main=v)
#     rug(obs[v,,])
#     abline(v=omean[v])
#     abline(v=omean[v]+osd[v], lty=2)
#     abline(v=omean[v]-osd[v], lty=2)
# }

## T700 is bimodal!  So that makes sigma weird.  Let's try interdecile
## range instead (10th to 90th percentiles).  We can also compare it
## to 2x IQR

opctl <- apply(obs, 1, quantile, na.rm=TRUE,
               probs=c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
iqr <- opctl["75%",] - opctl["25%",]

dev.new()
par(mfrow=c(3,3))
for(v in mvars){
    plot(density(obs[v,,], na.rm=TRUE), main=v)
    rug(obs[v,,])
    abline(v=omean[v])
    abline(v=opctl["50%",v], col=2)
    abline(v=opctl[c("25%","75%"),v], col=3)
    abline(v=opctl[c("10%","90%"),v], col=4)
    abline(v=opctl[c("5%","95%"),v], col=5)
    abline(v=omean[v]+iqr[v], lty=2)
    abline(v=omean[v]-iqr[v], lty=2)
}


## The interdecile range looks good and I can describe it using a
## single term, so let's use that.

idr <- opctl["90%",] - opctl["10%",]


## Update metrics to include MAE scaled by IDR

for(v in vars){
    mask <- metrics$vars == v
    metrics$err[mask] <- metrics$mae[mask]/idr[v]
}


metrics$cor[metrics$scen=="obs"] <- 1
metrics$mae[metrics$scen=="obs"] <- 0
metrics$err[metrics$scen=="obs"] <- 0

## Redo scatterplot with uniform axes


#png(file="plot/corr-mae-unif.png", width=6, height=6, units="in", res=180)
par(mfcol=c(3,3), mar=c(3.1, 3.1, 2, 1), mgp=c(1.5,0.5,0))

for (v in c("A850", "U850", "V850",
            "T700", "legend", "Z700",
            "S250", "U250", "V250")){
    if(v == "legend"){
        plot(c(0,1), c(0,1), pch=NA, ann=FALSE, axes=FALSE)
        legend("top", names(rsym), pch=rsym, ncol=2, cex=0.9)
        legend("bottomleft", names(gcol), fill=gcol, cex=0.8)
        legend("bottomright", lty=1:3, col=lleg, cex=0.8,
               c("dynamic", "spatial","point"))
    } else {
        m <- subset(metrics, vars==v & scen=="hist")
        plot(m$cor, m$err, xlim=c(-1,1), ylim=c(0,1),
             main=v, xlab="correlation", ylab="scaled error",
             pch=rsym[m$method], col=gcol[m$gcm], cex=1.3)

        for(g in names(gcol)){
            mm <- subset(m, gcm==g & method != "dummy")
            segments(mm[1,"cor"], mm[1,"err"], mm[-1,"cor"], mm[-1,"err"],
                     col=lcol[g], lty=c(1,1,2,2,2,3,3))
        }
    }
}
#dev.off()


## Now redo metric and barplot.  Now that everything is on the same
## scale, I think we can just do Manhattan distance from the lower
## right corner.

metrics$cred <- (metrics$cor - metrics$err + 2)/3

cred <- xtabs(cred ~ gcm+method, data=metrics, subset=metrics$scen=="hist")
cred <- cred[c("HadGEM","GFDL","MPI"),rev(methods[-1])] / 10

dev.new()
par(mfrow=c(1,2))
barplot(cred, beside=TRUE, horiz=TRUE, las=1, xlim=c(0,1),
        col=gcol, main="credibilty metric")

barplot(t(cred), beside=TRUE, horiz=TRUE, las=1, xlim=c(0,1),
        col=rep(gcol,each=ncol(cred)), main="credibilty metric")

## Okay!  I think that looks better.  However, I'm probaby
## overweighting winds, and I think I need to do proper vector
## correlations.

library(fields)
source("plotfun.R")

zr <- c(-10,10)

mag <- function(x,y){sqrt(x^2+y^2)}

Uobs <- obs["U250",,]
Vobs <- obs["V250",,]

Umod <- mod[9,"U250",,]
Vmod <- mod[9,"V250",,]

Uerr <- Umod - Uobs
Verr <- Vmod - Vobs

THobs <- atan2(Uobs, Vobs)
THmod <- atan2(Umod, Vmod)

UVcor <- cos(THmod - THobs)

Mobs <- mag(Uobs, Vobs)
Mmod <- mag(Umod, Vmod)
Merr <- mag(Uerr, Verr)

## hack to set uniform arrow scale
Uobs[1,1] <- 10; Vobs[1,1] <- 0
Umod[1,1] <- 0;  Vmod[1,1] <- 10
Uerr[1,1] <- Vmod[1,1] <- -sqrt(10)

par(mfrow=c(2,2))

image.plot(dlon, dlat, mag(Uobs,Vobs), main="obs 250", zlim=zr)
vectorfield(dlon, dlat, Uobs, Vobs, lwd=2, col=1)

image.plot(dlon, dlat, mag(Umod,Vmod), main="mod 250", zlim=zr)
vectorfield(dlon, dlat, Umod, Vmod, lwd=2, col=1)

image.plot(dlon, dlat, mag(Uerr,Verr), main="err 250", zlim=zr)
vectorfield(dlon, dlat, Uerr, Verr, lwd=2, col=1)

image.plot(dlon, dlat, UVcor, main="cos theta 250", zlim=c(-1,1))

## mean(UVcor) is the coefficient of correlation for a vector field
## mean(Merr) is the MAE for the vector field

## So that gives us:

## U850, V850: vector corr, mean(merr)
## U250, V250: vector corr, mean(merr)
## T700: cor(), scaled MAE

## Z700: cor(), scaled MAE
## Q850: cor(), scaled MAE
## A850: cor(), scaled MAE

## A850 arguably overcounts moisture transport, but that's the most
## important process here, so I think we should include it.

## Of course, that also gives us 6 panels + legend, which is a pain.
## So let's see if it makes a big difference...

## S250: skip; covered by U250/V250
## Z500: skip; highly correlated with Z700

############################################

## So do I update my scatter plots, or just the metrics barplot?

## Update scatter.  Include Z500, and add a map of the region the
## metrics are calculated over.

## A850	T700	w250
## w850	Z700	legend
## Q850	Z500	map

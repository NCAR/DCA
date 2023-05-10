library(abind)

source("names.R")
source("util.R")
source("~/climod/R/renest.R")

load("data/rdata/misc.Rdata")
load("data/rdata/ua.meta.Rdata")



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
        metrics[id,"mae"] <- sum(abs(obs[v,,] - mod[m,v,,]), na.rm=TRUE)
    }
}

metrics$cor <- round(metrics$cor, digits=3)
metrics$mae <- round(metrics$mae, digits=3)

write.csv(metrics, file="plot/corr.wet.delta.csv", row.names=FALSE)

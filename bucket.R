load("data/prec.Rdata")
source("names.R")

buckets <- c("dry", "moist", "wet")
trace <- 0.254
theta <- 3


## convert here to propagate through everything else

for(p in periods){
    prec[[p]]$month <- month.abb[prec[[p]]$month]
}


## categorize precip amounts

pcut <- list()
for(p in c("hist","rcp85")){
    df <- prec[[p]][,-(1:7)]     #need to drop non-precip columns
    pcut[[p]] <- lapply(df, cut, breaks=c(-Inf,trace,theta,Inf),
                        labels=buckets, include.lowest=TRUE)
}
pcut[["obs"]] <- list(obs=cut(prec$obs$obs,
                          breaks=c(-Inf,trace,theta,Inf),
                          labels=buckets, include.lowest=TRUE))

bprec <- mapply(cbind, lapply(prec, `[`, 1:7), pcut) |>
    lapply('rownames<-', NULL)

## Note: operating directly on columns -(1:7) (i.e., w/o intermediate
## pcut & df) is orders of magnitude slower.



## calculate stats by bucket

percentage <- function(x){100*x/sum(x)}

bstat <- list(count=list(), pct=list())

for(p in periods){
    ## split by month, px, py
    psplit <- split(bprec[[p]], ~ px + py + month)

    ## tabulate buckets
    pcount <- lapply(psplit, `[`, -(1:7)) |>
        lapply(apply, 2, table)

    ## corresponding metadata
    meta <- lapply(psplit, `[`, i=1:3, j=c("period","month","px","py"))
    
    ## stich 'em together.  (Note: exact form matters a lot here)
    bstat$count[[p]] <- mapply(cbind, meta, bucket=list(buckets), pcount,
                               SIMPLIFY=FALSE, USE.NAMES=FALSE,
                               MoreArgs=list(row.names=NULL)) |>
                                   do.call(what=rbind)

    ## and do the same for percentages by bucket
    ppct <- lapply(pcount, function(x){apply(x, 2, percentage)})

    bstat$pct[[p]] <- mapply(cbind, meta, bucket=list(buckets), ppct,
                               SIMPLIFY=FALSE, USE.NAMES=FALSE,
                               MoreArgs=list(row.names=NULL)) |>
                                   do.call(what=rbind)    
}


save(file="data/buckets.Rdata", buckets, trace, theta, bprec, bstat)

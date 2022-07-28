load("plot/cmaps.Rdata")
load("data/prec.Rdata")
source("names.R")

buckets <- c("dry", "moist", "wet")
trace <- 0.254
theta <- 3

## categorize precip amounts
pcut <- lapply(prec, cut, breaks=c(-Inf,trace,theta,Inf),
               labels=buckets, include.lowest=TRUE)

## put resulting factor back into array form
pb <- mapply(array, pcut, lapply(prec, dim), lapply(prec, dimnames))


## bucket counts by month
bcount <- lapply(pb, apply, c("method","px","py","month"), table) |>
    lapply(setname, "bucket", "ndn", 1)

## bucket percentages
percentage <- function(x){x/sum(x)}
bpct <- lapply(bcount, apply, c("method","px","py","month"), percentage)


bstat=list(count=bcount, pct=bpct)
bprec=pb

save(file="data/buckets.Rdata", buckets, trace, theta, bprec, bstat)

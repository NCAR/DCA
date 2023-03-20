## Statistical downscaling methods for precip (other than SDSM)
library(MBC)
source("names.R")
load("data/prec.SGP.Rdata")


## raw data by gcm, month, & loc; each method takes as input one such
## subset df (possibly with corresponding obs split) & returns a
## similarly-shaped df with new method.

pr <- list()


pr$obs <- split(prec[prec$scen == "obs",], ~ month + locname)
pr$raw <- split(prec[prec$method =="raw",], drop=TRUE,
                ~ month + locname + gcm)
## gcm last for proper ordering for mapply arg recycling


## shuffle data by month & year
dummy <- function(mdf, ..., seed=222){
    set.seed(seed)
    result <- mdf
    result$prec <- split(mdf$prec, mdf$year) |>
        lapply(sample) |>
        unsplit(mdf$year)
    result$method <- "dummy"
    return(result)
}

## simple bc: threshold at value that matches obs wet/dry ratio,
## then scale so means match; apply same to future

trim <- function(x, th){x[x < th] <- 0; x}

simple <- function(mdf, odf){
    obs <- odf$prec
    cur <- mdf[mdf$scen == "hist","prec"]
    fut <- mdf[mdf$scen != "hist","prec"]

    ratio <- sum(obs > 0) / length(obs)
    theta <- quantile(cur, 1-ratio)

    tcur <- trim(cur, theta)
    tfut <- trim(fut, theta)

    sigma <- mean(obs) / mean(tcur)
    scur <- tcur * sigma
    sfut <- tfut * sigma

#    ## check
#    plot(sort(obs)^0.25)
#    points(sort(cur)^0.25, col='blue')
#    points(sort(fut)^0.25, col='red')
#    abline(v=(1-owdrat)*length(obs))
#    abline(h=sigma^0.25)
#    points(sort(scur)^0.25, col='blue', pch=3)
#    points(sort(sfut)^0.25, col='red', pch=3)
    
    result <- mdf
    result[result$scen == "hist","prec"] <- scur
    result[result$scen != "hist","prec"] <- sfut
    result$method <- "simple"
    return(result)
}

## using Cannon's QDM instead of KDDM to match NA-CORDEX BC

qdm <- function(mdf, odf){
    obs <- odf$prec
    cur <- mdf[mdf$scen == "hist","prec"]
    fut <- mdf[mdf$scen != "hist","prec"]

    bc <- QDM(obs,cur,fut, ratio=TRUE, trace=0.254)

    result <- mdf
    result[result$scen == "hist","prec"] <- bc$mhat.c
    result[result$scen != "hist","prec"] <- bc$mhat.p
    result$method <- "qdm"
    return(result)
}

## (Dropping mBCN and GLM as out of scope because time)

## And now we apply it to all of the data!

## This could be done with chained apply fn's, but looping lets us
## track progress

## method ordering for viz/analysis purposes

methods <- c("qdm", "simple", "dummy")

for(method in methods){
    pr[[method]] <- list()
    cat("\n=====\n", method, "\n=====\n")
    for(gcm in levels(prec$gcm)[-4]){
        cat("\n-----\n", gcm, "\n-----\n")
        for(loc in unique(prec$locname)){
            cat(loc,": ")
            for(month in 1:12){
                cat(month, " ")
                oid <- paste(month, loc, sep='.')
                mid <- paste(oid, gcm, sep='.')                
                pr[[method]][[mid]] <- do.call(method,
                                               list(mdf=pr$raw[[mid]],
                                                    odf=pr$obs[[oid]]))
            }
            cat("\n")
        }
    }
}

sdprec <- lapply(pr[methods], do.call, what=rbind) |>
    do.call(what=rbind)

## need to add levels to method factor, want to keep obs as last element

dlev <- levels(prec$method)

newlevs <- c(head(dlev, -1), methods, tail(dlev, 1))

prec$method   <- factor(  prec$method, lev=newlevs)
sdprec$method <- factor(sdprec$method, lev=newlevs)

prec <- rbind(prec, sdprec)


save(prec, file="data/prec.SGP.all.Rdata")

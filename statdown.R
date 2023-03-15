## Statistical downscaling methods for precip (other than SDSM)
source("names.R")
load("data/prec.SGP.Rdata")


## raw data by gcm, month, & loc; each method takes as input one such
## subset df (possibly with corresponding obs split) & returns a
## similarly-shaped df with new method.

pr <- list()

pr$obs <- split(prec[prec$scen == "obs",], ~ locname + month)
pr$raw <- split(prec[prec$method =="raw",], drop=TRUE,
                ~ gcm + locname + month)


## shuffle data by month & year
dummy <- function(df, ..., seed=222){
    set.seed(seed)
    result <- df
    result$prec <- split(df$prec, df$year) |>
        lapply(sample) |>
        unsplit(df$year)
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


## Some methods need other variables, either surface or UA
## So probably need to run the above extraction with a different set of
## inputs and variables
## 
## kddm: by month & loc
## 
## MBCn: by month & loc (needs other surface vars)
## 
## GLM: by month & loc (needs UA vars for this loc)
## ? https://github.com/SantanderMetGroup/downscaleR
## Note: this is a perfect-prog downscaling method


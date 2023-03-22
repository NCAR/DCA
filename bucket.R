load("data/prec.SGP.all.Rdata")
source("names.R")

buckets <- c("dry", "moist", "wet")
trace <- 0.254
theta <- 3

bucketize <- function(x){
    cut(x, breaks=c(0, trace, theta, Inf), labels = buckets, right=FALSE)
}

prec$bucket <- bucketize(prec$prec)
                   

## calculate stats by bucket

percentage <- function(x){100*x/sum(x)}

same <- function(x){all(x == x[1])}

calc <- function(df, column="bucket"){
    keep <- apply(df, 2, same)
    count <- table(df[[column]])
    pct <- percentage(count)
    
    cbind(df[1,keep] |> setname(NULL, "row"),
          data.frame(factor(names(count)), unclass(count), unclass(pct)) |>
          setname(c(column, "count","pct"), "col")
          ) |>
        setname(NULL, "row")
}

#system.time(
bstat <- split(prec, ~ month + gcm + method + scen + locname, drop=TRUE) |>
    lapply(calc) |> do.call(what=rbind) |> setname(NULL, "row")
#)


save(file="data/buckets.Rdata", buckets, trace, theta, bucketize, bstat)

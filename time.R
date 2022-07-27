library(devtools)
load_all("~/climod")
library(PCICt)
library(abind)

source("names.R")


## etime: elapsed time since epoch
## (e.g., unix seconds since 1970-01-01)

## nctime: netcdf etime, defined by @units attribute in format
## "[units] since [ISO-8601 date]"


nctime2ymd <- function(nctime){
    ustring <- strsplit(nctime@units, " ") |> unlist()
    etime2ymd(etime=nctime,
              calendar=nctime@calendar,
              epoch=ustring[3],
              units=ustring[1])              
}


## Convert etime to YMD; drops any HMS component
## Return value: n x 3 char array

etime2ymd <- function(etime,
                      calendar="gregorian",
                      epoch="1970-01-01",
                      units=c("seconds","minutes","hours","days")){
    
    tosec <- switch(match.arg(units),
                    seconds=1,
                    minutes=60,
                    hours=60*60,
                    days=60*60*24)
    
    as.PCICt.numeric(x=etime*tosec, cal=calendar, origin=epoch, tz="GMT") |>
        as.character() |>
            gsub(pat=" .*", rep="") |> 
                strsplit('-') |>
                    do.call(what=rbind) |>
                        setname(c("year","month","day"), "col")
}


## convert 1D timeseries to 3D year-month-day array

foldtime <- function(x, ymd){
    class(ymd) <- "numeric"

    # convert year values to 1:N
    oyear <- min(ymd[,'year']):max(ymd[,'year'])
    ymd[,'year'] = ymd[,'year'] - min(oyear) + 1

    z <- array(NA, dim=c(31,12,length(oyear)),
               dimnames=list(day=sprintf("%02i",1:31),
                   month=month.abb,
                   year=oyear))

    z[ymd[,c("day","month","year")]] <- x
    return(z)
}

## checks:
# foldtime(0:600, etime2ymd(0:600,"gregorian","1980-01-01","day"))
# foldtime(0:600, etime2ymd(0:600,"noleap","1980-01-01","day"))
# foldtime(0:600, etime2ymd(0:600,"360_day","1980-01-01","day"))



## fold time on last dimension of N-dimensional array
foldtimeND <- function(x, ymd){    
    if(ndim(x) < 2){
        foldtime(x, ymd)
    } else {
        z <- asplit(x, 1) |>
            lapply(foldtimeND, ymd) |>
                abind(along=0, use.dnns=TRUE)
        dimnames(z) <- c(head(dimnames(x),-1), tail(dimnames(z),3))
        return(z)
    }
}

## checks:
# x <- 0:700
# y <- rbind(x,x+1000)
# z <- abind(y+1e4, y+2e4, y+3e4, along=0)
# dimnames(z) <- list(foo=c("a","b","c"),bar=c("i","j"),time=c())
# 
# xx <- foldtimeND(x, etime2ymd(0:700,"gregorian","1980-01-01","day"))
# yy <- foldtimeND(y, etime2ymd(0:700,"noleap","1980-01-01","day"))
# zz <- foldtimeND(z, etime2ymd(0:700,"360_day","1980-01-01","day"))


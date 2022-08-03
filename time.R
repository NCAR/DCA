library(devtools)
load_all("~/climod")
library(PCICt)
library(abind)

source("names.R")

## functions to change representation of time in data


## etime: elapsed time since epoch
## (e.g., unix seconds since 1970-01-01)

## cftime: CF-netcdf etime, defined by @units attribute in format
## "[units] since [ISO-8601 date]"

## ymdstr: time as YYYY-MM-DD or YYYY-mon-DD string 

## ymdarr: time as Nx3 numeric array with columns year, month, day

## fold time: convert length N timeseries to 3D array with dimensions
## day, month, and year.  (N.B.: day dim has size 31; missing/invalid
## dates are filled with NA.)


## Notes for YMD class:
## internal representation 3xN numeric array
## attribute fmt, default = %Y-%b-%d
## attribute cal, default = "standard"
## length.ymd <- function(x){nrow(x)}
## as.PCICt <- function(x){as.PCICt(as.character(x), format=x@fmt, cal=x@cal}
## as.character.ymd <- function(x){}
## ? print.ymd <- function(x){print(as.character(x))}



## Convert etime to YMD string; drops any HMS component

etime2ymdstr <- function(etime,
                         calendar="gregorian",
                         epoch="1970-01-01",
                         units=c("seconds","minutes","hours","days"),
                         format="%Y-%b-%d"){
    
    tosec <- switch(match.arg(units),
                    seconds=1,
                    minutes=60,
                    hours=60*60,
                    days=60*60*24)
    
    as.PCICt.numeric(x=etime*tosec, cal=calendar, origin=epoch, tz="GMT") |>
        as.character(format=format)
}

## checks:
#etime2ymdstr(0)                                    ## should return 1970-Jan-01
#etime2ymdstr(31, calendar="360_day", units="days")               ## 1970-Feb-02
#etime2ymdstr(28, cal="noleap", units="days", epoch="1984-02-01") ## 1984-Mar-01
#etime2ymdstr(15.5*24*60, units="minutes")                        ## 1970-Jan-16


## Convert CF-netcdf time to YMD string

cftime2ymdstr <- function(nctime, format="%Y-%b-%d"){
    ustring <- strsplit(nctime@units, " ") |> unlist()
    etime2ymdstr(etime=nctime,
                 calendar=nctime@calendar,
                 epoch=ustring[3],
                 units=ustring[1],
                 format=format)
}

#nt <- 1:3 + 90
#nt@units="days since 1980-01-01"
#nt@calendar="360_day"
#cftime2ymdstr(nt, "%Y/%m/%d")  ## 1980/04/02, 1980/04/03, 1980/04/04


## convert YMD string formats
ymdstr2date <- function(ymd,
                       infmt="%Y-%b-%d",
                       outfmt="%Y-%m-%d",
                       cal="standard"){
    as.PCICt(ymd, cal=cal, format=infmt) |> as.character(format=outfmt)
}

#ymdstr2str("2000-Feb-29")                           ## 2000-02-29
#ymdstr2str("1 Dec, 1856", "%d %b, %Y", "%m/%e/%y")  ## 12/ 1/56


## converts a YMD string vector to n x 3 char array, splitting on sep
ymdstr2arr <- function(ymd, sep='-'){
    strsplit(ymd, split=sep) |>
        do.call(what=rbind) |>
            setname(c("year","month","day"), "col")
}

# ymdstr2arr(etime2ymdstr(1:3*30-1, unit="day", cal="360"))
# ##      year   month day 
# ## [1,] "1970" "Jan" "30"
# ## [2,] "1970" "Feb" "30"
# ## [3,] "1970" "Mar" "30"


## convert 1D timeseries to 3D array[day, month, year]

foldtime <- function(x, ymdarr){
    
    class(ymdarr) <- "numeric"

    # convert year values to 1:N
    oyear <- min(ymdarr[,'year']):max(ymdarr[,'year'])
    ymdarr[,'year'] = ymdarr[,'year'] - min(oyear) + 1

    z <- array(NA, dim=c(31,12,length(oyear)),
               dimnames=list(day=sprintf("%02i",1:31),
                   month=month.abb,
                   year=oyear))

    z[ymdarr[,c("day","month","year")]] <- x
    return(z)
}

## checks:

# base <- 0:600
# 
# gregor <- etime2ymdstr(base,"gregorian","1980-01-01","day","%Y-%m-%d")
# noleap <- etime2ymdstr(base,"noleap",   "1980-01-01","day","%Y-%m-%d")
# hadley <- etime2ymdstr(base,"360_day",  "1980-01-01","day","%Y-%m-%d")
# 
# foldtime(base, ymdstr2arr(gregor))
# foldtime(base, ymdstr2arr(noleap))
# foldtime(base, ymdstr2arr(hadley))



## convert 3D array[day,month,year] to 1D timeseries at dates in ymdarr
## (can't just do dim(x)<-NULL because of NA-padding on day dimension)
## ymd indexes need to match dimnames on x

unfoldtime <- function(x, ymdarr){
    class(ymd) <- "numeric"
    N <- nrow(ymdarr)
    
    
}



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


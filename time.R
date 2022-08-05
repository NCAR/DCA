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

## ymd: time as YYYY-MM-DD or YYYY-mon-DD string 



## Convert etime to YMD string; drops any HMS component

etime2ymd <- function(etime,
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

cftime2ymd <- function(nctime, format="%Y-%b-%d"){
    ustring <- strsplit(nctime@units, " ") |> unlist()
    etime2ymd(etime=nctime,
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
ymdreformat <- function(ymd,
                        infmt="%Y-%b-%d",
                        outfmt="%Y-%m-%d",
                        cal="standard"){
    as.PCICt(ymd, cal=cal, format=infmt) |> as.character(format=outfmt)
}

#ymdstr2str("2000-Feb-29")                           ## 2000-02-29
#ymdstr2str("1 Dec, 1856", "%d %b, %Y", "%m/%e/%y")  ## 12/ 1/56


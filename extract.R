## Extracting point data from netcdf
## 
## >> replaces the surface part of ingest.R
## 
## 
## inputs:  a point location or set of point locations
## 	 paths (on glade) to RCM, GCM, LOCA, CNN data
## 
## output: a dataframe with extracted precip data for the location(s):
## 
## columns:
## year num
## month num
## day num
## gcm (obs = ERAI)
## method (obs = gridMET)
## scen (obs = obs)
## locname (e.g., SGP-100-36)
## lon
## lat
## prec

library(ncdf4)
source("time.R")

## for general use, this all should go in a config file of some kind
basedir <- "/glade/work/mcginnis/DCA2/data/surface"

obs <- "ERAI.gridMET.obs"

gcms <- c("GFDL","HadGEM","MPI")
dmethods <- c("raw","RegCM4","WRF","CNN","LOCA") ## dynamical methods
scenarios <- c("hist", "rcp85")

## for now, assume the following folder structure
## ideally, this should also go in a config file

combos <- expand.grid(gcms, dmethods, scenarios)
ids <- apply(combos, 1, paste, collapse='.') 

rownames(combos) <- ids
colnames(combos) <- c("gcm","method","scen")

## add in a row for obs data
obits <- unlist(strsplit(obs,'.',fixed=TRUE)) |> setname(colnames(combos))
for(i in names(obits)){
  levels(combos[[i]]) <- c(levels(combos[[i]]), obits[i])
}
combos[obs,] <- obits

indirs <- paste0(basedir, '/', ids) |> gsub(pat='.', rep='/', fixed=TRUE)
names(indirs) <- ids

## this too should go into a config file / command arg

vars <- c("pr", "prec")
vpat <- "pr.*nc"

## as should this

lats <- c(32, 34, 36, 38)
lons <- c(-102, -100, -98, -96, -94)


## Begin actual data ingestion

infiles <- sapply(indirs, dir, pattern=vpat, full=TRUE)
 

## open netcdf4 files without reading in everything
## takes a little while, b/c it does read in dims & metadata
nc <- lapply(infiles, nc_open, readunlim=FALSE)


## Everything is already on NAM-22i grid and subset to periods of
## interest, so this code is much simpler than it would need to be in
## the general case.

data <- list()

## stub: loop on vars goes here

for(tlon in lons){
  for(tlat in lats){

    locname <- paste0("SGP",tlon,"-",tlat)    
    cat(locname, "\n")
    
    for(id in ids){    
        nci <- nc[[id]]
        
        ## get indexes corresponding to target location
        ilon <- which.min(abs((tlon %% 360) - (nci$dim$lon$vals %% 360)))
        ilat <- which.min(abs(tlat - nci$dim$lat$vals))
    
        
        ## which variables are we ingesting?
        ivar <- vars[vars %in% names(nci$var)]
    
        ## ingest timeseries - dims go X-Y-Z-T
        pr <- ncvar_get(nci, ivar, start=c(ilon, ilat, 1), count=c(1,1,-1))
    
        ## cleanup
        if(ivar %in% c("pr","prec")){
            ## convert CNN NA to 0
            if(combos[id,"method"] == "CNN"){
                pr[is.na(pr)] <- 0
            }
            ## floor at zero
            pr <- pmax(pr, 0)
    
            ## check / convert units
            if(nci$var[[ivar]]$units == "kg m-2 s-1"){
                ## 1 kg H20 = 1 mm * 1 m^2, x sec/day gives mm/day
                pr <- pr * 24 * 60 * 60  
            }
                
            ## set obs values below trace to zero?
            pr[pr < 0.254] <- 0
        }
        
        ## get & convert time to date
        
        cftime <- ncvar_get(nci, "time")
        cftime@units <- nci$dim$time$units
        cftime@calendar <- nci$dim$time$calendar
    
        dd <- cftime2ymd(cftime, "%Y-%m-%d")
    
        ymd <- strsplit(dd, '-') |>
          renest() |>
            lapply(unlist) |>
              lapply(as.numeric) |>
                setname(c("year","month","day"))
    
    
        data[[id]] <- data.frame(cbind(ymd, combos[id,], locname,
                                       lon=tlon, lat=tlat, prec=pr))
        cat(id, "\t")
      }
    cat("\n")
  }
}
prec <- do.call(rbind, data)


save(file="../prec.SGP.Rdata", prec)

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
## date "YYYY-MM-DD"
## year num
## month num
## day num
## period (hist / rcp85 / (obs))
## lon
## lat
## name
## gcm
## <one column per method: raw / rcm1 / rcm2 / cnn / loca / etc.>
## 
## 
## [stub in extracting other vars]
## 

library(ncdf4)

## for general use, this should be a command-line parameter
basedir <- "/glade/work/mcginnis/DCA2/data/surface"

obspath <- paste0(basedir, "/ERAI/gridMET/obs")


gcms <- c("GFDL","HadGEM","MPI")
dmethods <- c("raw","RegCM4","WRF","CNN","LOCA") ## dynamical methods
periods <- c("hist", "rcp85")

## for now, assume the following folder structure
## ideally, this should go in a config file of some kind

indirs <- expand.grid(basedir, gcms, dmethods, periods) |>
    apply(1, paste, collapse='/')


## this too should go into a config file / command arg

vars <- c("pr", "prec")
vpat <- "pr.*nc"


infiles <- sapply(indirs, dir, pattern=vpat, full=TRUE)
 

## open netcdf4 files without reading in everything
## takes a little while, b/c it does read in dims & metadata
nc <- lapply(infiles, nc_open)  ## readunlim=FALSE ?


## Everything is already on NAM-22i grid and subset to periods of
## interest, so this code is much simpler than it would need to be in
## the general case.


## loop on locations, inputs [vars]

## hack stub

tlon <- -98
tlat <- 36

## get indexes corresponding to target location

ilon <- which.min(abs((tlon %% 360) - (foo$dim$lon$vals %% 360)))
ilat <- which.min(abs(tlat - foo$dim$lat$vals))


## which variables are we ingesting?
readme <- vars[vars %in% names(foo$var)]

## ingest timeseries - dims go X-Y-Z-T
timser <- ncvar_get(foo, readme, start=c(ilon, ilat, 1), count=c(1,1,-1))

## cleanup
## 	convert CNN NA to 0
## 	floor at zero
## 	check / convert units
## 	set obs values below trace to zero?
## 
## construct dataframe
## 
## create misc convenience variables (methods, perspan, etc.)
## 
## save to file
## 
## 
## Also, have a switch that does obs data (which has no methods,
## different period)
## 
## Do I want obs to be in the same file / dataframe?
## 
## 
## #####
## 
## Separately, we then also have a script to generate data for
## statistical methods
## 
## Reads in a precip dataframe
## Also wants corresponding obs dataframe
## 
## Some methods need other variables, either surface or UA
## So probably need to run the above extraction with a different set of
## inputs and variables
## 
## 
## dummy:  shuffle data points by month
## 
## simple: by month & loc:
## 	find threshold to match obs dry/wet ratio
## 	     set values below threshold to 0
## 	     (if too many dry, we're out of luck)
## 	find mean (median?) value of obs wet days
## 	     scale model wet days to match stat
## 	apply same transformation to future data
## 
## kddm: by month & loc
## 
## MBCn: by month & loc (needs other surface vars)
## 
## GLM: by month & loc (needs UA vars for this loc)
## ? https://github.com/SantanderMetGroup/downscaleR
## Note: this is a perfect-prog downscaling method


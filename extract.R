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
## outline:
## 
## get paths (from config file? assumed folder structure w/ symlinks?)
## 
## open netcdf4 files
## >> can I not read in the entire file, just the points?
## 
## loop on locations, inputs [vars]
## ingest timeseries
## 
## cleanup
## 	convert CNN NA to 0
## 	floor at zero
## 	check units
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


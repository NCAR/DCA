## Calculate variance for UA variables by method, month

source("names.R")

## Overall variance will be dominated by the seasonal cycle, so it's
## not very interesting.  Look only at monthly values.

## Ignoring buckets until we determine we need them

outdir <- "data/var"
system(paste("mkdir -p", outdir))

indir <- "data/rdata"
infiles <- dir(indir, pattern="ua\\..*\\..*\\..*\\.Rdata")

mnum <- paste0("m", sprintf("%02d",1:12)) |> setname(month.abb)


for(i in 1:length(infiles)){

    print(infiles[i])
    load(paste0(indir,"/",infiles[i]))    # ~3 sec/file
  
    ## unwrap list - don't use unlist(); it flattens to vector
    id <- names(ua)
    ua <- ua[[1]]    

    ## indexing
    dates <- dimnames(ua)$date
    midx <- lapply(month.abb, grep, dates) |> setname(month.abb)

    ## monthly variance
    monvar <- list()

    for(m in month.abb){
        monvar[[m]] <- apply(ua[,,,midx[[m]]], 1:3, var, na.rm=TRUE)
    }
    
    # ## quick check:
    # dev.new()
    # par(mfrow=c(3,4))
    # for(m in month.abb) image(monvar[[m]]["T700",,], main=m)
    # mtext("T700 monthly variance", outer=TRUE, side=3, line=-1.5)
    
    outfile <- gsub("^ua", "ua.var", infiles[i])
    save(file=paste0(outdir,"/", outfile), monvar)
}

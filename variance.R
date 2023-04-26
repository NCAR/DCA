## Calculate variance for UA variables by method, month

source("names.R")
source("~/climod/R/renest.R")

## function to simplify GCM names
gsimp <- function(x){
    gsub(x, pat="2", rep="") |> gsub(pat="-.*?\\.", rep=".")
}


## Overall variance will be dominated by the seasonal cycle, so it's
## not very interesting.  Look only at monthly values.

## Ignoring buckets until we determine we need them


## TRUE = plot onscreen, FALSE = plot to file
test = FALSE


outdir <- "data/var"
plotdir <- "plot/upper"
system(paste("mkdir -p", outdir, plotdir))

indir <- "data/rdata"
infiles <- dir(indir, pattern="ua\\..*\\..*\\..*\\.Rdata")

mnum <- paste0("m", sprintf("%02d",1:12)) |> setname(month.abb)

vardata <- list()

for(i in 1:length(infiles)){

    print(infiles[i])
    load(paste0(indir,"/",infiles[i]))    # ~3 sec/file


    ## need clean data upstream
    clean <- function(x){x[abs(x)>1e20]<-NA; return(x)}

    ## unwrap list - don't use unlist(); it flattens to vector
    id <- names(ua) |> gsimp()
    ua <- ua[[1]] |> clean()

    ## need to fix this upstream, too
    if(grepl("WRF", id)){
        ua[c("Z700","Z500"),,,] <- ua[c("Z700","Z500"),,,] / 9.8 
    }
        
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

    vardata[[id]] <- monvar
    
#    outfile <- gsub("^ua", "ua.var", infiles[i])
#    save(file=paste0(outdir,"/", outfile), monvar)
}

save(file=paste0(outdir,"/ua.var.Rdata"), vardata)


## And now plot...

library(pals)
source("plotfun.R")
load("data/rdata/misc.Rdata")
load("data/rdata/ua.meta.Rdata")
load("plot/cmaps.Rdata")


#for(m in 1:12){
for(m in 5){
    mnum <- sprintf("m%02d",m)
    mon <- month.abb[m]

    mplotdir <- paste(plotdir, mnum, sep='/')
    
    vdata <- renest(vardata)[[mon]] |> lapply(sqrt)

    vmap <-list()
    for(v in vars){ vmap[[v]] <- cubicl(256)}

    ## plotting limits
    xr <- c(-135,-55)
    yr <- c(20,60)
    bounds <- list(lon=xr, lat=yr)


    vmeta <- strsplit(names(vdata), '.', fixed=TRUE) |>
        do.call(what="rbind") |>
        data.frame() |>
        setname(c("gcm","method","scen")) |>
        setname(names(vdata), "row")

    ## factors ordered for sorting
    vmeta$scen   <- factor(vmeta$scen, levels=scen)
    vmeta$gcm    <- factor(vmeta$gcm, levels=gcms)
    vmeta$method <- factor(vmeta$method, levels=methods)

    vmeta <- vmeta[with(vmeta, order(scen, gcm, method)),]

    ## plot ranges
    vblock <- abind(vdata, along=0)
    nonfut <- grep("rcp85", names(vdata), inv=TRUE, val=TRUE)
    vlim <- apply(vblock[nonfut,,,], 2, zerange) |>
        as.data.frame() |> as.list()

    for(meth in dynmethods){

        ## which ones to plot
        plotids <- rownames(vmeta)[with(vmeta, ( scen=="obs" |
                                                 (scen=="hist" &
                                                  method==meth)))]
        
        gnames <- strsplit(plotids, '.', fixed=TRUE) |> sapply('[',1)
        gnamelist <- list(gcm=gnames, var=NULL, lon=NULL, lat=NULL)

        plotdata <- abind(vdata[plotids], along=0, use.dnns=TRUE,
                          new.names=gnamelist)

        if(test){
            dev.new(width=15, height=5.5)
        } else {
            outfile <- paste("monstdev", meth, mnum, "png", sep='.')
            png(file=paste0(mplotdir, "/", outfile),
                width=15, height=5.5, units="in", res=120)
        }
        
        main <- paste(meth, mon, "baseline upper atmosphere stdev, hist")
        gridmap(lon, lat, plotdata, mapcol='black', zlims=vlim,
                cmaps=vmap, units=uaunits, main=main)

        if(!test){ dev.off() }
    }
    
}

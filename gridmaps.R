## Make grid plots of climatology, anomaly, etc.

library(abind)

load("data/misc.Rdata")
load("data/buckets.Rdata")
load("plot/cmaps.Rdata")
source("plotfun.R")
source("names.R")

plotdir <- "plot/upper"
system(paste("mkdir -p", plotdir))

## plotting limits
#xr <- c(-150,-50)
#yr <- c(15,65)
xr <- c(-135,-55)
yr <- c(20,60)
bounds <- list(lon=xr, lat=yr)

## load data into a list instead of the global environment
listload <- function(filepath){
    load(filepath, temp_env <- new.env())
    as.list(temp_env)
}


## calculate moisture advection
## abind::asub lets us not care how many dimensions there are
calcA <- function(arr, Dim=1){
    u <- asub(arr, "U850", Dim)
    v <- asub(arr, "V850", Dim)
    q <- asub(arr, "Q850", Dim)
    a <- q * sqrt(u^2 + v^2)
    abind(arr, A850=a, along=Dim, use.dnns=TRUE)
}

total <- listload("data/MPI/anom/total.Rdata") |> rapply(calcA, how="replace")

vars <- c(vars, "A850")
uaunits["A850"] <- paste(uaunits["Q850"], uaunits["U850"])



## looping goes here

x = -98
y = 36
xx = "x098"
yy = "y36"
mon = "May"
mm = "m05"

loc = paste0(xx, '.', yy)

mplotdir <- paste(plotdir, mm, sep='/')
system(paste("mkdir -p", mplotdir))


modper <- setdiff(periods, "obs")  ## no obs data for buckets

suffix <- paste("",mm,xx,yy,"Rdata", sep=".")
infiles <- paste0("data/",gcm,"/anom/sgp/",loc,"/",modper,suffix)

bdata <- list()
bdata <- lapply(infiles, listload) |> setname(modper)


## crop UA data to CONUS region so that plotted z-ranges are correct

uaconus <- rapply(total, crop, sub=bounds, how="replace")
clon <- lon[lon %within% xr]
clat <- lat[lat %within% yr]

## baselines

ocf <- c("obs","hist","rcp85")  ## plotting order

dev.new(width=13.5, height=4)

basedata <- abind(uaconus$baseline[ocf], along=0, use.dnns=TRUE) |>
    setname("period", "ndn")

main <- "MPI baseline  upper atmosphere annual climatology"
gridmap(clon, clat, basedata, mapcol='black',
        cmaps=climap, units=uaunits, main=main)


dev.copy2pdf(file=paste0(mplotdir,"/baseline.",mm,".pdf"),
             width=13.5, height=4, title=main)


## May moisture advection plot

maydata <- abind(uaconus$totclim[ocf], along=0, use.dnns=TRUE)[,,,,"May"] |>
    setname("period", "ndn")

facets <- as.data.frame(
    cbind(
        raster  = list("A850",           "T700", "Z500"),
        vector  = list(c("U850","V850"), NULL,   c("U250","V250")),
        contour = list(NULL,             "Z700", NULL),
        title   = list(
            qflux  = "850-mb moisture advection",
            p700   = "700-mb temperature + geopotential",
            hicirc = "250-mb wind + 500-mb geopotential")
    )
)


dev.new(width=14, height=9)

main=paste(gcm, "upper atmosphere", mon, "climatology")
gridmap(clon, clat, maydata, facets, cmaps=climap, units=uaunits,
        main=main, mapcol="black")

dev.copy2pdf(file=paste0(mplotdir,"/advection.",mm,".pdf"),
             width=14, height=9, title=main)


## bucketized climatology

## 
## clim = by month & bucket  (clim/total = maydata above)
## anom = clim minus baseline
## delta = bucket anomaly - total anomaly

    
bplotdir <- paste(mplotdir, loc, sep='/')
system(paste("mkdir -p", bplotdir))



## crop UA data to CONUS & calculate q-flux
bconus <- rapply(bdata, crop, sub=bounds, how="replace", classes="array") |>
    rapply(calcA, how="replace", classes="array", Dim=3)

## symmetric z-ranges around zero
blim <- apply(bconus$hist$delta, 3, srange, simplify=FALSE)


## short titles
bfacets <- facets
bfacets$title <- c("850-mb Qflux", "700-mb T & Z", "high circul'n")


## all methods by bucket

for(b in buckets){
    dev.new(width=7, height=12)
    main <- paste(gcm, "UA", mon, b, "anomaly difference")
    ambb <- bconus$hist$delta[b,,,,]

    gridmap(clon, clat, ambb, bfacets, cmaps=anomap, zlim=blim,
            units=uaunits, mapcol='black', main=main, alen=0,
            arrowcol='darkgray', concol="darkgray", concex=0.6)

    dev.copy2pdf(file=paste0(bplotdir,"/anom.bucket.",mm,".",b,".pdf"),
             width=7, height=12, title=main)
}



## all buckets by method

testpt <- list(x=x, y=y, pch=23, col="black", bg="red")

for(m in methods){
    dev.new(width=14, height=9)
    main <- paste(gcm,"+",m, "UA", mon, "anomaly difference")
    abbm <- bconus$hist$delta[,m,,,]

    gridmap(clon, clat, abbm, facets, cmaps=anomap, zlim=blim,
            units=uaunits, mapcol='black', main=main, pointargs=testpt,
            arrowcol='dimgray', concol="dimgray", concex=0.8)

    dev.copy2pdf(file=paste0(bplotdir,"/anom.method.",mm,".",m,".pdf"),
                 width=14, height=9, title=main)
}


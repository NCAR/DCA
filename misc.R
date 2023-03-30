## generate miscellaneous useful helper variables

load("data/buckets.Rdata")

## move obs to beginning for plotting convenience
shift <- function(x){c(tail(x,1), head(x,-1))}

gcms <- c("ERAI", "HadGEM", "MPI", "GFDL")  ## ECS ordering
methods <- shift(levels(bstat$method))
dynmethods <- c("raw","RegCM4","WRF")
scen <- shift(levels(bstat$scen))

locs <- unique(bstat$loc)
plon <- unique(bstat$lon)
plat <- unique(bstat$lat)

locmap <- unique(bstat[,c("locname","lon","lat")])
rownames(locmap) <- locmap$locname
locmap$locname <- NULL
colnames(locmap) <- c("x","y")

save(file="data/rdata/misc.Rdata", buckets, trace, theta,
     gcms, methods, dynmethods, scen, locs, plon, plat, locmap)


## generate miscellaneous useful helper variables

load("data/prec.SGP.all.Rdata")

## move obs to beginning for plotting convenience
shift <- function(x){c(tail(x,1), head(x,-1))}

gcms <- c("ERAI", "HadGEM", "MPI", "GFDL")
methods <- shift(levels(prec$method))
dynmethods <- c("raw","RegCM4","WRF")
scen <- shift(levels(prec$scen))

locs <- unique(prec$loc)
plon <- unique(prec$lon)
plat <- unique(prec$lat)

save(file="data/rdata/misc.Rdata",
     gcms, methods, dynmethods, scen, locs, plon, plat)


library(abind)

## TODO: make this work

# .checkargs <- function(){
#     parent <- parent.frame()
#     miss <- eval(missing(range), parent)
#     cat("args", range, lower, upper, "\n")
#     cat(miss, is.finite(lower), is.finite(upper))
#     if(!missing(range)){
#         stopifnot(length(range)==1)
#         if(!missing(lower) || !missing(upper)){
#             stop("can't specify both range and lower/upper")
#         } else {
#             lower <- range[1]
#             upper <- range[2]
#         }
#     }
# 
#     if(!is.numeric(lower) || !is.numeric(upper)){
#         stop("non-numeric limits")
#     }
#         
#     if(upper < lower) stop("upper must be >= lower")
#     assign("lower", lower, parent.frame())
#     assign("upper", lower, parent.frame())
# }
# 
# foo <- function(range=NULL, lower=-Inf, upper=Inf){
#     .checkargs()
# #    .checkargs(range, lower, upper)
#     cat("rlu", range, lower, upper)
# }


## subset an array with coordinate dimensions
#crop <- function(z, xr=NULL, yr=NULL, dnn=list(x="lon", y="lat")){
#    .checkargs()

# @param x: an array to be cropped

# @param sub: a named list of bounds; element name = name of dim,
#      element value = range of coords to keep along that dim.

crop <- function(z, sub){
    for(d in names(sub)){
        coord <- as.numeric(dimnames(z)[[d]])
        ind <- coord %within% sub[[d]]
        dind <- which(names(dimnames(z)) == d)
        z <- asub(z, ind, dind)
    }
    return(z)
}


## discard non-finite & values outside range
trim <- function(x, range=NULL, lower=-Inf, upper=Inf,
                 finite=TRUE, inclusive=TRUE){
#    .checkargs()
    if(!missing(range)){ lower<-range[1]; upper<-range[2] }
    if(inclusive){
        x <- x[x >= lower & x <= upper]
    } else {
        x <- x[x > lower & x < upper]
    }
    
    if(finite) x <- x[is.finite(x)]
    return(x)
}

## set values outside range to limit of range
clamp <- function(x, range=NULL, lower=-Inf, upper=Inf, na=NULL, nan=na){
#    .checkargs()
    if(!missing(range)){ lower<-range[1]; upper<-range[2] }

    ## order matters - NaN is NA, but NA is not NaN
    if(!missing(nan)) x[is.nan(x)] <- nan
    if(!missing(na))  x[is.na(x)]  <- na

    return(pmin(upper, pmax(lower, x)))
}

## rescale values to [0,1] range
unitize <- function(x){
    (x - min(x, na.rm=TRUE)) / diff(range(x, finite=TRUE))
}


## range() with correct default for na.rm, which I get tired of typing.
narange <- function(...){range(..., na.rm=TRUE)}


## symmetric range around zero
srange <- function(...){c(-1,1)*max(abs(narange(...)))}


## narange floored at zero
zerange <- function(x){c(0,max(x, na.rm=TRUE))}


## which values fall between the bounds?
`%within%` <- function(x, bounds){
    x <= max(bounds) & x >= min(bounds)
}


## midpoints of vector values
mids <- function(x){
    stopifnot(all(x == sort(x)))
    (head(x,-1) + tail(x,-1))/2
}


## transpose row/column for list of vectors
## (i.e., get list by fields of strsplit output...)

transpose <- function(x){
    do.call(rbind, x) |> t() |> split(1:length(x[[1]]))
}


## get coordinate variable values, when coordinate variable is stored
## as array dimnames
cvar <- function(x, dname){
    as.numeric(dimnames(x)[[dname]])
}

## get values of z-array closest to target x & y coordinates
nearest <- function(tx, ty, z, x=cvar(z, xname), y=cvar(z, yname),
                    xname="lon", yname="lat", lonmod=TRUE){
    ## wrap lon values to 0:360
    if(lonmod){
        tx <- tx %% 360
        x <- x %% 360
    }

    if(tx < min(x) | tx > max(x) | ty < min(y) | ty > max (ty)){
        stop("target coordinates are outside array coverage")
    }
    
    ix <- which.min(abs(x - tx))
    iy <- which.min(abs(y - ty))
    asub(z, ix, xname) |> asub(iy, yname)
}


## load data into a list instead of the global environment
listload <- function(filepath){
    load(filepath, temp_env <- new.env())
    as.list(temp_env)
}


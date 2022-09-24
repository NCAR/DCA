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
unitize <- function(x){(x - min(x)) / diff(range(x, finite=TRUE))}


## range() with correct default for na.rm, which I get tired of typing.
narange <- function(...){range(..., na.rm=TRUE)}


## symmetric range around zero
srange <- function(...){c(-1,1)*max(abs(narange(...)))}


## which values fall between the bounds?
`%within%` <- function(x, bounds){
    x <= max(bounds) & x >= min(bounds)
}


## transpose row/column for list of vectors
## (i.e., get list by fields of strsplit output...)

transpose <- function(x){
    do.call(rbind, x) |> t() |> split(1:length(x[[1]]))
}

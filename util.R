## clean data - drop NA & values outside range
## TODO: clamp vs trim
## TODO: inclusive / exclusive range
# clean <- function(x, range=c(lower,upper), lower=-Inf, upper=Inf){
# 
#     ## argument checking
#     if(!all(sapply(c(lower,upper,range), is.numeric))){
#         stop("non-numeric limits")
#     }
# 
#     if(!missing(range) && (!missing(lower) || !missing(upper))){
#         stop("can't specify both range and lower/upper")
#     }
# 
#     if(!missing(range)){
#         # if we're here, lower & upper must be default
#         stopifnot(length(range)==2)
#         lower <- range[1]; upper <- range[2]
#     }
#     
#     if(upper < lower) stop("upper must be > lower")
# 
#     x[is.finite(x) & x >= lower & x <= upper]
# }


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


## range() with correct default for na.rm
narange <- function(...){range(..., na.rm=TRUE)}


## symmetric range around zero
srange <- function(...){c(-1,1)*max(narange(...))}


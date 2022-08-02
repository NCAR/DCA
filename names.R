
## Create a named list from arguments, taking names from args

namelist <- function(...){
    ## with a<-1; b<-2; namelist(a,b):
    ##         [[1,2]]     `list(a,b)`  [[list,a,b]]  [[a,b]]
    names <- list(...) |> substitute() |> as.list() |> tail(-1)
    list(...) |> setNames(names)
}


## Generic pipelineable name-setting function
## because you can't do "f(x) |> colnames()<-value"

## vec:                names(object) <- nm
## row:             rownames(object) <- nm
## col:             colnames(object) <- nm
## all:             dimnames(object) <- nm
## dim:      dimnames(object)[[dim]] <- nm
## ndn: names(dimnames(object))[dim] <- nm

setname <- function(object, nm,
                    ntype=c("vec","row","col","all","dim","ndn"), dim=1){
    switch(match.arg(ntype),
           vec = {   names(object) <- nm},
           row = {rownames(object) <- nm},
           col = {colnames(object) <- nm},
           all = {dimnames(object) <- nm},
           dim = {dnm <- dimnames(object)
                  dnm[[dim]] <- nm
                  dimnames(object) <- dnm
              },
           ndn = {names(dimnames(object))[dim] <- nm}
           )
    return(object)
}


## convenience function

dimnamename <- function(x){names(dimnames(x))}

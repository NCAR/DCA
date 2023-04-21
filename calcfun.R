library(abind)


## calculate moisture advection
## abind::asub lets us not care how many dimensions there are
calcA <- function(arr, Dim=1){
    u <- asub(arr, "U850", Dim)
    v <- asub(arr, "V850", Dim)
    q <- asub(arr, "Q850", Dim)
    a <- q * sqrt(u^2 + v^2)
    abind(arr, A850=a, along=Dim, use.dnns=TRUE)
}

## calculate wind speed
calcS <- function(arr, Dim=1){
    u <- asub(arr, "U250", Dim)
    v <- asub(arr, "V250", Dim)
    s <- sqrt(u^2 + v^2)
    abind(arr, S250=s, along=Dim, use.dnns=TRUE)
}



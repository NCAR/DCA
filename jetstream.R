
###############################
## Proper jetstream calculation


## numerical integration using Simpson's Rule:
## https://en.wikipedia.org/wiki/Simpson's_rule

## given diff(x0,x1,x2) = (h,h), integral of F from x0 to x2 ~=
## h/3 * (F(x0) + 4*F(x1) + f(x2)

## x = integrand; h = spacing between points.
simpson <- function(x, h){
    stopifnot(length(h)==1)

    n <- length(x)

    if(n<2){return(0)}

    if(n==2){return(h*sum(x)/2)}

    if(n %% 2){
        # n odd -> even number of intervals
        k <- c(1, 4, rep(c(2,4),(n-1)/2 -1), 1)
        return(h/3 * sum(x*k))
    } else {
        # n even -> odd number of intervals
        # do the first three intervals using Simpson's 3/8 rule
        return(h*3/8 * sum(c(1,3,3,1)*x[1:4]) + simpson(x[4:n], h))
    }
}


## apply f to x[1], x[1:2], x[1:3] ... x[n]
cumfun <- function(x, f, ...){
    g <- function(index,var){var[index]}
    n <- length(x)
    lapply(1:n, seq) |>
        lapply(FUN=g, var=x) |>
        sapply(f, ...) 
}


## Quick way to deal with rcm missing values in lower corners: flip
## field around before integrating

flip <- function(M){
    apply(M,2,rev) |> apply(1, rev) |> t()
}


## Proper jetstream calculation: integrate the velocity field using
## Simpson's Rule to get the vector potential psi; streamlines are
## then contours of constant psi.
## U = d psi/dy & V = - d psi/dx

## Use transform=flip for missing values in lower corners

streamfunction <- function(U, V, lon, lat, transform=I, ...){
    dx <- mean(diff(lon))
    dy <- mean(diff(lat))

    dn <- dimnames(U)

    U <- transform(U, ...)
    V <- transform(V, ...)

    psi <- t(dy * apply(U, 1, cumfun, simpson, h=dy)) -
             dx * apply(V, 2, cumfun, simpson, h=dx)
    dimnames(psi) <- dn

    psi <- transform(psi, ...)
    return(psi)
}

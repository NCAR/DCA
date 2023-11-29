## Functions for calculating and visualizing the jetstream.

## Simple approximation.  Starts with a point at the middle of each
## gridcell in the leftmost column.  Moves from left to right,
## calculating where the point would move vertically in a single step
## to the next column, doing linear interpolation in the y-direction.

## Also accumulates the total speed along each stream so that you can
## pick the fastest one.


simplejet <- function(U, V, lon, lat){
    streams  <- list()
    cumspeed <- c()

    ## assuming they are arranged for image-type plotting 
    nx <- length(lon)
    ny <- length(lat)
    
    stopifnot(identical(dim(U), dim(V)) |
              identical(dim(U) , c(nx, ny)))


    for(y in 1:ny){
        ss <- sqrt(U[1,y]^2 + V[1,y]^2)
        sy <- i <- y

        for(x in 2:nx){

            north <- ceiling(i)
            south <- floor(i)

            sfrac <- i %% 1
            nfrac <- 1 - sfrac
        
            unorth <- U[x,north]
            vnorth <- V[x,north]
            usouth <- U[x,south]
            vsouth <- V[x,south]
            
            ubar <- unorth * nfrac + usouth * sfrac
            vbar <- vnorth * nfrac + vsouth * sfrac

            ss[x] <- sqrt(ubar^2 + vbar^2)
            sy[x] <- sy[x-1] + vbar / ubar

            # stop when streams run off grid
            if(sy[x] < 1 || sy[x] > ny){ break }
        }
        cumspeed[y] <- sum(ss)
        streams[[y]] <- sy        
    }

    # ## check: convert ragged list into matrix & plot
    # S <- lapply(streams, \(s){length(s)<-nx;s}) |>
    #     do.call(what=cbind)
    # matplot(S, type="l", lwd=3)
    
    ## convert index values into lat-lon coordinates

    jets <- list()
    for(i in 1:ny){
        s <- streams[[i]]
        jets[[i]] <- list(x = lon[1:length(s)],
                          y = approx(1:ny, lat, s)$y)
    }
    
    attr(jets, "cumspeed") <- cumspeed
    return(jets)
}

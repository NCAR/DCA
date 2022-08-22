
load("data/ua.Rdata")
#load("data/misc.Rdata")
#load("data/buckets.Rdata")
#source("names.R")


## PCA doesn't care about the spatial dimension of the data; you give
## it an array with one dimension for space and another for time, do
## the SVD math, and then rearrange the 1D eigenvectors back into
## fields to get your EOFs.  To do multivariate EOF (MEOF) analysis,
## all you do is normalize each variable to get them all on the same
## scale, then concatenate them together along the non-time dimension
## and proceed as normal.

## And to unwind all those non-time dimensions, as long as time is the
## *last* dimension, all we need to do is change dim(array) from
## (nv,nx,ny,nt) to (nv*nx*ny, nt), and vice-versa to get it back.


hist <- ua$hist

uadim <- dim(hist)
nd <- length(uadim)

nt <- uadim[nd]
nvxy <- prod(uadim[-nd])

dim(hist) <- c(nvxy, nt)

#for(N in 2^(2:14)){
#    cat("\n",N,'\t')
#    cat(system.time(pca <- prcomp(hist[1:N,], scale=TRUE)))
#    plot(pca, main=N)
#}

## So it can do 2^14 = 16k x-y-var timeseries in 2 hours.  Which is
## not so bad; it can probably do the whole set in 5-6 hours, then.

system.time(pca <- prcomp(hist, scale=TRUE))


## basically ALL of the variance is explained by the first mode, which
## I'm guessing means that I need to remove the annual cycle.

## probably also need to scale the entire field, rather than scaling
## each gridcell+var separately (which is what the default scale=TRUE
## does.

## And if I split it into months, I'll drop the data size by a factor
## of 12, which should speed it up to only about 3-4 minutes per month,
## or about 45 minutes for the whole thing, rather than several hours.
## It also removes the annual cycle in an en passant kind of way, so I
## don't have to bother with doing it manually.


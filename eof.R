load("data/ua.Rdata")
#load("data/misc.Rdata")
#load("data/buckets.Rdata")
source("names.R")


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


## Running PCA on an entire 30-year 8-dim data slab takes a couple hours.

#hist <- ua$hist

dates <- dimnames(ua$hist)$date

## run PCA by month

#for(m in month.abb){
m <- "May"
p <- "hist"

uamon <- ua[[p]][,,,grep(m, dates)]


## dimensions for reshaping the array -- we can convert foo[v,x,y,t]
## to bar[vxy,t] by just changing dim(foo).

uadim <- dim(uamon)
nd <- length(uadim)

nt <- uadim[nd]
nvxy <- prod(uadim[-nd])


## standardize each variable

mu <- apply(uamon, 1, mean)
sigma <- apply(uamon, 1, sd)


zmon <- sweep(uamon, "var", mu, '-') |> sweep("var", sigma, '/')

dim(zmon) <- c(nvxy, nt)
dimnames(zmon)<-list(z=NULL, t=NULL)



## run PCA - only takes ~75 seconds per month (on monthly data)

system.time(
    pca <- prcomp(zbar, scale=FALSE, center=FALSE)
    )


## scree plot

plot(pca$sd)
plot(pca$sd, log='xy')


## for the moment, let's keep the top 12 components (those with
## eigenvalues greater than 3).  In actuality, looking at the log-log
## plot, I think it's got 1/x-ish scaling from around component 20 to
## component 100 or 150, so we should probably be keeping the first 20
## or so.  Values above 2 gives us the first 25 components, so maybe
## that's good.

eof <- array(pca$x, dim=dim(uamon),
             dimnames=c(dimnames(uamon)[-nd], list(pc=NULL)))[,,,1:12]

stop()

source("plotfun.R")

gridmap(lon, lat, eof)


## rotate components to spread out variance



#}



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


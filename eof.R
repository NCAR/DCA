load("data/ua.Rdata")
library(nFactors)
#load("data/misc.Rdata")
#load("data/buckets.Rdata")
source("names.R")
source("plotfun.R")


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



## need to divide EOFs by corresponding eigenvalues and rearrange
## dimensions for plotting with gridmap

pcnm <- paste("PC", 1:nt)


eof <- array(pca$x, dim=dim(uamon),
             dimnames=c(dimnames(uamon)[-nd], list(pc=pcnm))) |>
    sweep("pc", pca$sdev, "/") |> 
    aperm(c("pc","var","lon","lat"))


## visualize the first few EOFs


nviz <- 8
zr <- apply(eof[1:nviz,,,], 2, srange) |> as.data.frame() |> as.list()
#gridmap(lon, lat, eof[1:nviz,,,]) 
gridmap(lon, lat, eof[1:nviz,,,], cmaps=anomap, zlim=zr, main=paste("PCA",m,p))


## How many PCs to keep?

## scree plot

plot(pca$sd)
plot(pca$sd, log='xy')


## from the nFactors package, which calculates elbow location using
## several different methods

elbow <- nScree(pca$sdev)

dev.new()
parsave <- par(no.readonly=TRUE)  ## plot.nScree messes up text colors
plot(elbow, main=paste("scree test",m, p))
par(parsave)

npc <- median(unlist(elbow$Components))


## rotate components to spread out variance



#}


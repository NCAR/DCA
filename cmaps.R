library(pals)
library(colorspace)

N <- 256

tweak <- function(C, x){
    adjustcolor(C, offset=c(1,1,1,0)*x) |> setNames(names(C))
}


## colormap for absolute variable values
climap <- list(
    U850=brewer.puor(N),
    V850=brewer.piyg(N),
    Q850=rev(cubehelix(N)),
    T700=inferno(N),
    Z700=rev(kovesi.diverging_rainbow_bgymr_45_85_c67(N)),
    Z500=brewer.spectral(N),
    U250=parula(N),
    V250=brewer.piyg(N),
    A850=rev(cubehelix(N)),
    S250=rev(cubehelix(N))
    )  

## colormap for anomaly (zero-centered) variables
anomap <- list(
    U850=brewer.piyg(N),
    V850=brewer.puor(N),
    Q850=brewer.brbg(N),
    T700=rev(brewer.rdbu(N)),
    Z700=rev(brewer.spectral(N)),
    Z500=rev(brewer.spectral(N)),
    U250=brewer.piyg(N),
    V250=brewer.puor(N),
    A850=brewer.brbg(N),
    S250=c(kovesi.linear_kryw_5_100_c67(N/2),
           rev(kovesi.linear_bgyw_15_100_c67(N/2)))
)

## brown-green-blue base colormap for precip buckets

## Other options that look decent:
## R4[7,3,4], obs -0.2, rcp85 +0.2
## (peru, forestgreen, royalblue3), hist +0.1, rcp85 +0.3


buckets <- c("dry","moist","wet")
baseline <- palette.colors(8,"Paired")[c(8,4,2)] |> setNames(buckets)
bucketmap <- list(obs=tweak(baseline, -0.1),
                  hist=baseline,
                  rcp85=tweak(baseline, 0.2))

save(file="plot/cmaps.Rdata", climap, anomap, bucketmap)



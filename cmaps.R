library(pals)
library(colorspace)

N <- 256

tweak <- function(C, x){
    adjustcolor(C, offset=c(1,1,1,0)*x) |> setNames(names(C))
}


## colormap for absolute variable values
climap <- list(
    U850=brewer.piyg(N),
    V850=brewer.puor(N),
    Q850=rev(cubehelix(N)),
    T700=inferno(N),
    Z700=rev(kovesi.diverging_rainbow_bgymr_45_85_c67(N)),
    Z500=brewer.spectral(N),
    U250=rev(viridis(N)),
    V250=brewer.puor(N)
    )  

## colormap for anomaly (zero-centered) variables
anomap <- list(
    U850=brewer.piyg(N),
    V850=brewer.puor(N),
    Q850=brewer.brbg(N),
    T700=brewer.rdbu(N),
    Z700=brewer.spectral(N),
    Z500=brewer.spectral(N),
    U250=brewer.piyg(N),
    V250=brewer.puor(N)
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



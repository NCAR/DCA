load("data/prec.Rdata")


load("data/prec.Rdata")

may <- prec$obs[prec$obs$month==5,]

yr <- range(may$obs)

may$logp <- log10(may$obs)

#lyr <- c(0,log10(yr[2]))

N <- length(unique(may$date))
nxy <- length(px)*length(py)

dates <- unique(may$date)

psort <- array(dim=c(nxy, N))
colnames(psort) <- dates



for(d in dates){
    psort[,d] <- sort(may$logp[may$date==d])
#    psort[,d] <- sort(may$obs[may$date==d])
}

psort[psort<=0] <- NA

#mu <- apply(psort, 2, mean, na.rm=TRUE)
#psorted <- psort[,order(mu)]

psorted <- apply(psort, 2, sort, na.last=TRUE)

dev.new()
matplot(psorted, pch='.', type='l', lty=1, col=adjustcolor(1, alpha=0.1))

pstat <- list()

pstat$pmin <- apply(psorted, 2, min, na.rm=TRUE)
pstat$pmax <- apply(psorted, 2, max, na.rm=TRUE)
pstat$pmed <- apply(psorted, 2, median, na.rm=TRUE)
pstat$pbar <- apply(psorted, 2, mean, na.rm=TRUE)
pstat$rain <- apply(psorted, 2, function(x){1-sum(is.na(x))/nxy})

theta <- function(x, th=log10(3)){ sum(x > th, na.rm=TRUE)/length(x)}
pstat$over3 <- apply(psorted, 2, theta)

fix <- function(x){x[!is.finite(x)] <- NA; x}

pstat <- lapply(pstat, fix)


## from https://intro2r.com/simple-base-r-plots.html

panel.hist <- function(x, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...) {
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r <- abs(cor(x, y, use='pair'))
    txt <- format(c(r, 0.123456789), digits = digits)[1]
    txt <- paste0(prefix, txt)
    if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex.cor * r)
}


dev.new()
plot(as.data.frame(pstat), pch='.',
     diag.panel=panel.hist,
     upper.panel=panel.smooth,
     lower.panel=panel.cor,
     main="stats across 20 gridcells of log(precip) [values <=0 dropped]")
mtext(side=1, outer=TRUE, line=-1.5,
      '"rain" = fraction of gridcells with > 1 mm precip')


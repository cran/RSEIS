`jmlcrc` <-
function (tfrep, tfspec = numeric(dim(tfrep)[2]), bstep = 3, 
    iteration = 10000, rate = 0.001, seed = -7, nbclimb = 10, 
    flag.int = TRUE, chain = TRUE, flag.temp = FALSE) 
{
    tfspectrum <- tfspec
    d <- dim(tfrep)
    sigsize <- d[1]
    nscale <- d[2]
    beemap <- matrix(0, sigsize, nscale)
    sqmodulus <- Re(tfrep * Conj(tfrep))
    for (k in 1:nscale) sqmodulus[, k] <- sqmodulus[, k] - tfspectrum[k]
    dim(beemap) <- c(nscale * sigsize, 1)
    dim(sqmodulus) <- c(nscale * sigsize, 1)
    z <- .C("Sbee_annealing", as.double(sqmodulus), beemap = as.double(beemap), 
        as.single(rate), as.integer(sigsize), as.integer(nscale), 
        as.integer(iteration), as.integer(seed), as.integer(bstep), 
        as.integer(nbclimb), as.integer(flag.int), as.integer(chain), 
        as.integer(flag.temp), PACKAGE = "Rwave")
    beemap <- z$beemap
    dim(beemap) <- c(sigsize, nscale)
    if (dev.set() != 1) 
        image(beemap)
    beemap
}

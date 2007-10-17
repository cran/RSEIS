`jmlcfamily` <-
function (ccridge, bstep = 1, nbchain = 100, ptile = 0.05) 
{

 require("Rwave")
 
    d <- dim(ccridge)
    sigsize <- d[1]
    nscale <- d[2]
    threshold <- range(ccridge)[2] * ptile
    sz <- sigsize + 2
    chain <- matrix(-1, nbchain, sz)
    orderedmap <- matrix(0, sigsize, nscale)
    dim(chain) <- c(nbchain * sz, 1)
    dim(ccridge) <- c(nscale * sigsize, 1)
    dim(orderedmap) <- c(nscale * sigsize, 1)
    z <- .C("Scrazy_family", as.double(ccridge), orderedmap = as.single(orderedmap), 
        chain = as.integer(chain), chainnb = as.integer(nbchain), 
        as.integer(sigsize), as.integer(nscale), as.integer(bstep), 
        as.single(threshold), PACKAGE = "Rwave")
    orderedmap <- z$orderedmap
    chain <- z$chain
    dim(orderedmap) <- c(sigsize, nscale)
    dim(chain) <- c(nbchain, sz)
    nbchain <- z$chainnb
    chain <- chain + 1
    chain[, 2] <- chain[, 2] - 1
    list(ordered = orderedmap, chain = chain, nbchain = nbchain)
}

`jmlcwt` <-
function (input, noctave, nvoice = 1, w0 = 2 * pi, twoD = TRUE, 
    plot = TRUE) 
{
  require("Rwave")
  
    oldinput <- input
    isize <- length(oldinput)
    tmp <- jadjust.length(oldinput)
    input <- tmp$signal
    newsize <- length(input)
    pp <- noctave * nvoice
    Routput <- matrix(0, newsize, pp)
    Ioutput <- matrix(0, newsize, pp)
    output <- matrix(0, newsize, pp)
    dim(Routput) <- c(pp * newsize, 1)
    dim(Ioutput) <- c(pp * newsize, 1)
    dim(input) <- c(newsize, 1)
    z <- .C("Scwt_morlet", as.single(Re(input)), as.single(Im(input)), 
        Rtmp = as.double(Routput), Itmp = as.double(Ioutput), 
        as.integer(noctave), as.integer(nvoice), as.integer(newsize), 
        as.single(w0), PACKAGE = "Rwave")
    Routput <- z$Rtmp
    Ioutput <- z$Itmp
    dim(Routput) <- c(newsize, pp)
    dim(Ioutput) <- c(newsize, pp)
    i <- sqrt(as.complex(-1))
    if (twoD) {
        output <- Routput[1:isize, ] + Ioutput[1:isize, ] * i
        if (plot) {
            image(Mod(output), xlab = "Time", ylab = "log(scale)", 
                main = "Wavelet Transform Modulus")
        }
        output
    }
    else {
        Rtmp <- array(0, c(isize, noctave, nvoice))
        Itmp <- array(0, c(isize, noctave, nvoice))
        for (i in 1:noctave) for (j in 1:nvoice) {
            Rtmp[, i, j] <- Routput[1:isize, (i - 1) * nvoice + 
                j]
            Itmp[, i, j] <- Ioutput[1:isize, (i - 1) * nvoice + 
                j]
        }
        Rtmp + Itmp * i
    }
}

setPrePix<-function(R1 , tt, name, flag='K', col='blue'  )
{
#### prepare a set of predicted pix for marking in swig (WPX)
    
    N = length(tt)

    A = recdate(R1$jd, R1$hr, R1$mi, R1$sec+tt, R1$yr )
    
    Z =  setWPX(phase = rep(flag, N),
                col = rep(col, N), yr = A$yr, jd = A$jd,
                hr = A$hr, mi = A$mi, sec = A$sec,
                dur = rep(0, N),
                name = name,
                comp = rep('V', N),
                dispcomp = rep('V', N),
                onoff = rep(TRUE, N))
    
    return(Z)
}




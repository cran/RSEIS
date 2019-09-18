
AUGMENTbutfilt<-function(a, fl=0, fh=0.5, deltat=1, type="BP", proto="BU",
                npoles=5, chebstop=30.0, trbndw=0.3, RM=FALSE, zp=TRUE, pct=.1 )

{
####  filtering;  edge effects; augment time series; padding
#### avoid ugly edge effects when filtering
    
####  IDEA: take signal, pad by 10 percent on either side,
####  perform butterworth filter
####  return original length signal, filtered

    if(missing(pct) ) pct = .1
    n = length(a)

    nside = floor(pct*n)

    beg = a[1:nside]
    tail = a[(n-(nside-1)):n]
    
    A.aug = c(rev(beg), a, rev(tail) )

    A.out = butfilt(A.aug, fl=fl, fh=fh, deltat=deltat, type=type, proto=proto,
            npoles=npoles, chebstop=chebstop, trbndw=trbndw, RM=RM, zp=zp)

    ###  cut the trace back to original length
    b = A.out[(nside+1):(length(A.out)-nside)]

    return(b)
    

}

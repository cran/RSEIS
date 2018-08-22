posix2RSEIS<-function(p)
    {
#####  convert a posix date to an RSEIS list date-time
####  this assumes that the p is as.POSIXct
####  the function POSIXlt counts months from 0-11 ?
        ###  as well as julian days from 0
        z = as.POSIXlt(p)
        yr = z$year+1900
        mo= z$mon+1
        dom =  z$mday
        hr = z$hour
        mi = z$min
        sec = z$sec
        jd = z$yday+1

        return(list(yr=yr,jd=jd,mo=mo, dom=dom,
                    hr=hr, mi=mi, sec=sec) ) 
    }

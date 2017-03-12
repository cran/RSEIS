j2posix<-function(timeinput)
    {
###########   return a posix compatable date from an RSEIS date list
        jt1 =   recdatel(timeinput)

        gmo = getmoday(jt1$jd, jt1$yr )

        dat1 =  paste(formatC(jt1$yr, width=4),
            formatC(gmo$mo, width=2, flag=0),
            formatC(gmo$dom, width=2, flag=0), sep="-")

        tim1 = paste(formatC(jt1$hr, width=2, flag=0),
            formatC(jt1$mi, width=2, flag=0),
            formatC(jt1$sec, width=2, flag=0), sep=":")

        starttime <- as.POSIXct(paste(dat1, tim1, sep=' '),tz="GMT")
        return(starttime)

    }

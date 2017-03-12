convertATT<-function(at1, yr)
    {
#### convert a julian day+time to an RSEIS date list

        jt1 = RSEIS::recdate(jd = at1, hr = 0, mi = 0, sec = 0, yr = yr)
        gmo = getmoday(jt1$jd, jt1$yr)
        jt1$mo = gmo$mo
        jt1$dom = gmo$dom
        return(jt1)

    }

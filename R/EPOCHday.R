EPOCHday<-function(yr, mo=0, dom=0, jd=1, origyr=1972)
  {
    ########  return number of days since the origin
    if(missing(origyr)) { origyr=1972 }
    if(missing(jd)) {

      moday = list(mo=mo, dom=dom)
    }
    if(missing(mo) & missing(dom))
      {
        moday  = getmoday(jd, yr)
      }
    else
      {
        
        moday = list(mo=mo, dom=dom)

      }

    myr = min(yr, na.rm=TRUE)
    if(is.null(myr)) { return(list(jday=1, origyr=origyr)) }
    if( myr<origyr ){ origyr  = myr  }
    
    jj =tojul(yr, moday$mo, moday$dom)- tojul(origyr, 1, 1)+1


    #######   alternative way based on unix time codes
    ########  zz =  as.numeric(ISOdate(yr, moday$mo, moday$dom) - ISOdate(origyr, 1, 1))

    return(list(jday=jj, origyr=origyr))
  }

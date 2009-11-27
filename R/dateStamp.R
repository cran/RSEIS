`dateStamp` <-
function(datelist)
  {

if(is.null(datelist$msec)) { datelist$msec = rep(0, length(datelist$sec) ) }
    
    rd = recdate(datelist$jd , datelist$hr , datelist$mi , datelist$sec +datelist$msec /1000, yr=datelist$yr )
    sec = floor(rd$sec)
    msec = 1000*(rd$sec-sec)
    t1 =   (msec-floor(msec))/1000
    msec = floor(msec)
    
    ftime = paste(sep=":", rd$yr,
      formatC(rd$jd, wid=3 , flag = "0")  ,
      formatC(rd$hr, wid=2 , flag = "0"),
      formatC(rd$mi, wid=2 , flag = "0"),
      formatC(sec, wid=2 , flag = "0"),
      formatC(msec, wid=3 , flag = "0") )
    
    return(ftime)
    
  }


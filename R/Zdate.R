`Zdate` <-
function(info, sel, t1)
  {
    if(is.null(t1)) { t1 = 0 }
    rd = recdate(info$jd[sel], info$hr[sel], info$mi[sel], info$sec[sel]+info$msec[sel]/1000+info$t1[sel]-info$off[sel]+t1)
    sec = floor(rd$sec)
    msec = 1000*(rd$sec-sec)
    t1 =   (msec-floor(msec))/1000
    msec = floor(msec)
    
    ftime = paste(sep=":", info$yr[sel],
      formatC(rd$jd, wid=3 , flag = "0")  ,
      formatC(rd$hr, wid=2 , flag = "0"),
      formatC(rd$mi, wid=2 , flag = "0"),
      formatC(sec, wid=2 , flag = "0"),
      formatC(msec, wid=3 , flag = "0") )
    
    return(ftime)
    
  }


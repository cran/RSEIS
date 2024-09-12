`Zdate` <-
function(info, sel=1, t1=0, sep='_' )
  {
    if(missing(sel)) { sel = 1:length(info$jd) }
    if(missing(t1)) { t1 = 0 }
    
    if(is.null(t1)) { t1 = 0 }

    n = length(info$sec) 

    if( is.null(info$off) ) {   info$off  = rep(0,n) }
    if( is.null(info$msec) ) { info$msec = rep(0,n) }
    if( is.null(info$t1) ) {   info$t1   = rep(0,n) }
   
    rd = recdate(info$jd[sel], info$hr[sel], info$mi[sel], info$sec[sel]+info$msec[sel]/1000+info$t1[sel]-info$off[sel]+t1, yr=info$yr[sel])
    sec = floor(rd$sec)
    msec = 1000*(rd$sec-sec)
    t1 =   (msec-floor(msec))/1000
    msec = floor(msec)
    
    ftime = paste(sep=sep, rd$yr,
      formatC(rd$jd, width=3 , flag = "0")  ,
      formatC(rd$hr, width=2 , flag = "0"),
      formatC(rd$mi, width=2 , flag = "0"),
      formatC(sec, width=2 , flag = "0"),
      formatC(msec, width=3 , flag = "0") )
    
    return(ftime)
    
  }


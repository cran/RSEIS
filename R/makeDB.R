makeDB<-function(path, pattern="R", kind =1)
  {
    if(missing(kind)) { kind =1 }
    if(missing(pattern)) {pattern="R"  }
    
    LF1 = list.files(path =path, pattern=pattern)

    ADB = list(fn="", 
      yr=0,
      jd=0,
      hr=0,
      mi=0,
      sec=0,
      dur=0,
      t1=0,
      t2=0,
      sta="", 
      comp="", 
      origyr=0)

    N = 0
    for(i in 1:length(LF1))
      {
        LF2 = list.files(path =paste(sep="/", path, LF1[i]) , full.names=TRUE, recursive = TRUE)
        sinfo  =  getseisinfo(LF2, kind=kind)
        for(j in 1:length(sinfo))
          {
           REC = sinfo[[j]]
           N = N + 1
           ADB$fn[N] = REC$fn
           ADB$sta[N] = REC$sta
           ADB$comp[N] = REC$comp
           ADB$yr[N] = REC$DATTIM$yr
           ADB$jd[N] = REC$DATTIM$jd
           ADB$hr[N] = REC$DATTIM$hr
           ADB$mi[N] = REC$DATTIM$mi
           ADB$sec[N] = REC$DATTIM$sec+REC$DATTIM$msec/1000
           ADB$dur[N] = REC$DATTIM$dt*REC$N
           

          }

      }



    
    ADB$origyr = min(ADB$yr)

 eday = EPOCHday(ADB$yr, jd = ADB$jd, origyr = ADB$origyr)
    ADB$t1 = eday$jday + ADB$hr/24 + ADB$mi/(24 * 60) + ADB$sec/(24 *
        3600)
    ADB$t2 = ADB$t1 + ADB$dur/(24 * 3600)


    invisible(ADB)

    
  }

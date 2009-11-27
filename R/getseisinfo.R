getseisinfo<-function(fnames, kind = 1)
{
  #####   seismic data
  #################  read and return the header information only
  ####  get full traces with wiggles, use: JGET.seis
  if (missing(kind)) {
    kind = 1
  }
  GIVE = as.list(1:length(fnames))
  ii = 1
  DATIM = rep(0, length = 4)
  n = 1
  dt = 0.025
  sec = 0
  thesta = "XXXXX"
  thecomp = "XXXXX"
  for (i in 1:length(fnames)) {
    fn = fnames[i]
    infile = fn
    if (file.exists(infile) == FALSE) {
      print(paste(sep = " ", "file does not exist", fn))
      next
    }
    else
      {
      }


    if(kind==0)
      {
        DAT  = list()
        load(fn)
        aunits = "volts"
        DAT$units = aunits
        
        GIVE[[i]] = DAT
        
        next

      }
    
    
    barfa = .C("CALL_JSETSEIS", PACKAGE = "RSEIS", infile,
      as.integer(kind), as.integer(n), as.double(dt), as.integer(DATIM),
      as.double(sec), thesta, thecomp)
    N = barfa[[3]]
    dt = barfa[[4]]
    DATIM = barfa[[5]]
    sec = barfa[[6]]
    thesta = barfa[[7]]
    thecomp = barfa[[8]]
    if (kind == 2) {
      if (thesta == "-12345") {
        stn = parseFN2STA(infile)
        thesta = stn$sta
        thecomp = stn$comp
      }
    }


    md = getmoday(DATIM[2], DATIM[1])
    t1 = 0
    t2 = dt * (N - 1)
    tstart = list(yr = DATIM[1], jd = DATIM[2], mo = md$mo,
      dom = md$dom, hr = DATIM[3], mi = DATIM[4], sec = sec,
      msec = 0, dt = dt, t1 = t1, t2 = t2, off = 0)
    
    aunits = "volts"

    GIVE[[i]] = list(fn = fn, sta = thesta, comp = thecomp,
          dt = dt, DATTIM = tstart, N = N, units = aunits , amp=NULL)
  }
  invisible(GIVE)

}



`TAPER.SEISN` <-
function(TH, sel=1:length(TH$JSTR),  TAPER=0.1 )
  {
    ###  TH = seismic structure
    ###  sel = vector of selected time series in structure
   

    if(missing(sel)) { sel = 1:length(TH$JSTR) }
  
    if(missing(TAPER)) { TAPER = NULL }
    
    NEWH = TH

    if(is.logical(sel)) { sel = which(sel) }

    
    for(i in 1:length(sel))
      {
        ii = sel[i]
        
        y = TH$JSTR[[ii]]
        dt = TH$dt[ii]
        y = fixNA(y)
        ny = is.na(y)
        ry = y[!ny]

        ry = applytaper(ry, p=TAPER) 

      
        NEWH$JSTR[[ii]][!ny] =  ry
        
      }

    

    proc = paste(sep=" ", "TAPER.SEISN", TAPER) 
      NEWH$process =  c(NEWH$process, proc)
    
    invisible(NEWH)
  }


`FILT.SEISN` <-
function(TH, sel=1:length(TH$JSTR), FILT=list(ON=FALSE, fl=0.5, fh=7.0, type="HP", proto="BU"), TAPER=0.1, POSTTAPER=0.1)
  {
    ###  TH = seismic structure
    ###  sel = vector of selected time series in structure
    ###   FILT   filter defined by:
    ### FILT = list(ON=FALSE, fl=0.5, fh=1.0, type="LP", proto="BU")
    ### FILT = list(ON=FALSE, fl=0.5, fh=7.0, type="HP", proto="BU")
    
    ### FILT = list(ON=FALSE, fl=0.5, fh=1.0, type="LP", proto="BU")
    ### FILT = list(ON=FALSE, fl=0.5, fh=7.0, type="HP", proto="BU")
    
    ### FILT = list(ON=FALSE, fl=6.0 , fh=20.0, type="BP", proto="BU")
    ### FILT = list(ON=FALSE, fl=0.5, fh=7.0, type="HP", proto="BU")

    if(missing(sel)) { sel = 1:length(TH$JSTR) }
    if(missing(FILT)) { FILT = list(ON=FALSE, fl=0.5, fh=7.0, type="HP", proto="BU")  }
    if(missing(TAPER)) { TAPER = NULL }
    if(missing(POSTTAPER)) { POSTTAPER = NULL }
    
    NEWH = TH
    
    for(i in 1:length(sel))
      {
        ii = sel[i]
        
        y = TH$JSTR[[ii]]
        dt = TH$dt[ii]
        ny = is.na(y)
        ry = y[!ny]
        if(!is.null(TAPER)) { ry = applytaper(ry, p=TAPER) }
        fy = butfilt(ry,FILT$fl, FILT$fh , dt, FILT$type , FILT$proto )
        ## ex = dt*0:(length(y)-1)
        ##  plot(ex, fy, type='l')
        if(!is.null(POSTTAPER)) { fy = applytaper(fy, p=POSTTAPER) }
        
        NEWH$JSTR[[ii]][!ny] =  fy
        
      }

    NEWH$filter = FILT
    invisible(NEWH)
  }


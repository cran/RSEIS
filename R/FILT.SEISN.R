`FILT.SEISN` <-
    function(TH, sel=1:length(TH$JSTR), FILT=list(ON=TRUE, fl=0.5, fh=7.0, type="HP",
                                                  proto="BU", RM=FALSE, zp=TRUE ), TAPER=0.1,
             POSTTAPER=0.1, AUGMENT=FALSE )
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
    if(missing(FILT)) { FILT = list(ON=TRUE, fl=0.5, fh=7.0, type="HP", proto="BU", RM=FALSE, zp=TRUE)  }
    if(missing(TAPER)) { TAPER = NULL }
    if(missing(POSTTAPER)) { POSTTAPER = NULL }

    if(is.null(FILT$RM)) { FILT$RM = FALSE }
     if(is.null(FILT$zp)) { FILT$zp = TRUE }
   
    if(is.null(FILT$npoles)) { FILT$npoles = 2 }
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

        if(!is.null(TAPER)) { ry = applytaper(ry, p=TAPER) }

        Fhi = FILT$fh
        Fnyq = 1/(2*dt) 

        if( Fhi > Fnyq )  { Fhi = Fnyq }

        if(AUGMENT){
            fy = AUGMENTbutfilt(ry, fl=FILT$fl, fh=Fhi , deltat=dt, type=FILT$type ,
                            proto=FILT$proto, npoles=FILT$npoles, RM=FILT$RM, zp=FILT$zp   )
        }
        else{
            fy = butfilt(ry, fl=FILT$fl, fh=Fhi , deltat=dt, type=FILT$type ,
                         proto=FILT$proto, npoles=FILT$npoles, RM=FILT$RM, zp=FILT$zp   )
        ## ex = dt*0:(length(y)-1)
        ##  plot(ex, fy, type='l')
        if(!is.null(POSTTAPER)) { fy = applytaper(fy, p=POSTTAPER) }
        }
        NEWH$JSTR[[ii]][!ny] =  fy
        
      }

    NEWH$filter = FILT

      proc = paste(sep=" ", "FILT.SEISN", FILT$fl, FILT$fh, FILT$type,
                   FILT$proto, TAPER, POSTTAPER) 
   NEWH$process =  c(NEWH$process, proc)
    
    invisible(NEWH)
  }


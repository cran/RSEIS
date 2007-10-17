`DISPLACE.SEISN`<-
function(TH, sel=1:length(TH$JSTR), inst=1, Kal=Kal, FILT=list(ON=TRUE, fl=1/30, fh=7.0, type="HP", proto="BU") )
{
  ########  convert the seismic to displacement
  if(missing(Kal)) {   Kal = PreSet.Instr() }
  if(missing(inst)) {   inst = rep(1,length=length(TH$JSTR))  }

  if(missing(sel)) { sel = 1:length(TH$JSTR) }
    if(missing(FILT)) { FILT = list(ON=TRUE, fl=1/30, fh=7.0, type="HP", proto="BU")  }
 
  Calibnew = c(1,1.0, 0.0 )

  
 for(i in 1:length(sel))
   {
     ii = sel[i]
     dt = TH$dt[ii]
     ins = inst[ii]
     if(ins==0) next
     
     y = TH$JSTR[[ii]]


     
     y = y-mean(y)
     y = detrend(y)
     y = applytaper(y)
     dy  = deconinst(y, dt, Kal, ins, Calibnew, waterlevel=1.e-8)
     
     ty = applytaper(dy-mean(dy), p=0.05)
     tapy = detrend(ty)
     fy = tapy-mean(tapy)

     #####  use this to get micro meters
     ## amp = fy*1000000
     amp = fy
     damp  = trapz(amp, dt)
     fy = butfilt(damp, FILT$fl, FILT$fh , dt, FILT$type , FILT$proto )
     TH$JSTR[[ii]] = fy
      TH$units[ii] = "m"
   }
 invisible(TH)
}


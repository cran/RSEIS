rseis2sac<-function(GH, sel=1, win=c(0,1), dir=".", BIGLONG=FALSE )
  {
##############  convert an RSEIS structure to SAC format
########   if dir is provided, the individual files will be
#########    written in that directory
    
    if(missing(sel)) { sel = 1:length(GH$STNS) }

    
    theENDIAN =  .Platform$endian

    aunits = "volts"

    if( !identical(dir, ".")) 
      {
        OLDir = getwd()
        dir.create(dir)
        setwd(dir)
      }

    for(j in 1:length(sel))
      {
        i = sel[j]
        
        fn = "GH"
        thesta = GH$STNS[i]
        thecomp = GH$COMPS[i]
        dt = GH$dt[i]
        
        tstart = list(yr=GH$info$yr[i]  ,
          jd=GH$info$jd[i]  ,
          mo=GH$info$mo[i]  ,
          dom=GH$info$dom[i]  ,
          hr=GH$info$hr[i]  ,
          mi=GH$info$mi[i]  ,
          sec=GH$info$sec[i]  ,
          msec=GH$info$msec[i]  ,
          dt=GH$info$dt[i]  ,
          t1=GH$info$t1[i]  ,
          t2=GH$info$t2[i]  ,
          off=GH$info$off[i]  
          )
        N = GH$info$n[i]

        sig = GH$JSTR[[i]]
        
        a1 = list(fn=fn, sta=thesta,  comp=thecomp, dt=dt, DATTIM=tstart,
          N=N, units=aunits , amp=sig , IO=list(kind=2, Iendian=theENDIAN,  BIGLONG=BIGLONG))
        write1sac(a1, BIGLONG=BIGLONG  , fn=NULL)
      }



    if( !identical(dir, ".")) 
      {
        
        setwd(  OLDir)
      }

  }

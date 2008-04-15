dogetsac<-function(fnames)
  {

    g = list()
    for(i in 1:length(fnames))
      {
        ZZ = ZGET.sac(fnames[i])
        dt = ZZ$dubs[ which(ZZ$dubnames=='delta') ]
        
        yr = ZZ$mints[which(ZZ$mintnames=='nzyear')]
        jd= ZZ$mints[which(ZZ$mintnames=="nzjday")]
        hr=  ZZ$mints[which(ZZ$mintnames=="nzhour")]
        mi=  ZZ$mints[which(ZZ$mintnames=="nzmin")]
        sec= ZZ$mints[which(ZZ$mintnames=="nzsec")]
        msec=ZZ$mints[which(ZZ$mintnames=="nzmsec")]
        evla= ZZ$dubs[ which(ZZ$dubnames=='evla') ]
        evlo= ZZ$dubs[ which(ZZ$dubnames=='evlo') ]
        evel= ZZ$dubs[ which(ZZ$dubnames=='evel') ]
        evdp= ZZ$dubs[ which(ZZ$dubnames=='evdp') ]
        stla= ZZ$dubs[ which(ZZ$dubnames=='stla') ]
        stlo= ZZ$dubs[ which(ZZ$dubnames=='stlo') ]

        stel= ZZ$dubs[ which(ZZ$dubnames=='stel') ]
        stdp= ZZ$dubs[ which(ZZ$dubnames=='stdp') ]
        
        
        sec = sec+msec/1000
        DATTIM=list(yr=yr, jd=jd, hr=hr, mi=mi, sec=sec)

       stn=ZZ$mchars[which(ZZ$charnames=="kstnm")] 
        compn=ZZ$mchars[which(ZZ$charnames=="kcmpnm")]

        comp = substr(compn, 1,1)
        sta = substr(stn, 1,3)
        
        A = prep1wig(wig =ZZ$x, sta=sta, dt=dt, comp=comp, units="unknown") 


        A[[1]]$DATTIM$yr=DATTIM$yr
        A[[1]]$DATTIM$jd=DATTIM$jd
        A[[1]]$DATTIM$hr=DATTIM$hr
        A[[1]]$DATTIM$mi=DATTIM$mi
        A[[1]]$DATTIM$sec=DATTIM$sec

        A[[1]]$fn = fnames[i]
        
        g = c(g, A)


      }


    PH1=prepSEIS(g)

    return(PH1)

  }




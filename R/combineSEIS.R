`combineSEIS` <-
function(IH, IV)
{
    ####  combine two GH lists for seismic analysis

  tAH = IH$info$jd + IH$info$hr/24+ IH$info$mi/(24*60)+(IH$info$sec+IH$info$msec/1000+IH$info$t1-IH$info$off)/(24*3600)
  tAV = IV$info$jd + IV$info$hr/24+ IV$info$mi/(24*60)+(IV$info$sec+IV$info$msec/1000+IV$info$t1-IV$info$off)/(24*3600)

  
  tmall = min(c(tAH, tAV))

  AsecH = (tAH-tmall)*(24*3600)
  AsecV = (tAV-tmall)*(24*3600)



  tBH = tAH + IH$info$n * IH$info$dt/(24*3600)
  tBV = tAV + IV$info$n * IV$info$dt/(24*3600)

  txall = max(c(tBH, tBV))

  BsecH = (txall-tBH)*(24*3600)
  BsecV = (txall-tBV)*(24*3600)



  for(j in 1:length(IH$JSTR))
    {	
######   the NA padding of traces occurs here
      t1 = AsecH[j]

      if(t1>0)
        {
          frontadd = rep(NA, length(seq(from=0, to=t1, by=IH$dt[j]  )))

        }
      else
        {
          frontadd =  NULL
        }
      t2 = BsecH[j]

      if(t1>0)
        {
          backadd = rep(NA,  length(seq(from=0, to=BsecH[j] , by=IH$dt[j]  ))  )

        }
      else
        {
          backadd =  NULL
        }
      amp=c( frontadd , IH$JSTR[[j]], backadd )
      IH$JSTR[[j]] = amp	
    }	


  for(j in 1:length(IV$JSTR))
    {	
######   the NA padding of traces occurs here
      t1 = AsecV[j]

      if(t1>0)
        {
          frontadd = rep(NA, length(seq(from=0, to=t1, by=IV$dt[j]  )))

        }
      else
        {
          frontadd =  NULL
        }
      t2 = BsecV[j]

      if(t1>0)
        {
          backadd = rep(NA,  length(seq(from=0, to=BsecV[j], by=IV$dt[j]  ))  )

        }
      else
        {
          backadd =  NULL
        }
      amp=c( frontadd , IV$JSTR[[j]], backadd )
      IV$JSTR[[j]] = amp	
    }	



  IH$info$off = IH$info$off+(tAH-tmall)*(24*3600)
  IV$info$off = IV$info$off+(tAV-tmall)*(24*3600)




  ginfo = list(fn=c(IH$info$fn,IV$info$fn), name=c(IH$info$name,IV$info$name), yr=c(IH$info$yr,IV$info$yr), jd=c(IH$info$jd,IV$info$jd), mo=c(IH$info$mo,IV$info$mo), dom=c(IH$info$dom,IV$info$dom),    hr=c(IH$info$hr,IV$info$hr),     mi=c(IH$info$mi,IV$info$mi),     sec=c(IH$info$sec,IV$info$sec),    msec=c(IH$info$msec,IV$info$msec), dt=c(IH$info$dt,IV$info$dt),     t1=c(IH$info$t1,IV$info$t1), t2=c(IH$info$t2,IV$info$t2), off=c(IH$info$off,IV$info$off),   n1=c(IH$info$n1,IV$info$n1),     n2=c(IH$info$n2,IV$info$n2),     n3=c(IH$info$n3,IV$info$n3),     n=c(IH$info$n,IV$info$n)) 


  GFIL = list(JSTR=c(IH$JSTR, IV$JSTR), STNS=c(IH$STNS, IV$STNS), dir=c(IH$dir, IV$dir), ifile=c(IH$ifile, IV$ifile), 
    COMPS=c(IH$COMPS, IV$COMPS), OCOMPS=c(IH$OCOMPS, IV$OCOMPS),dt=c(IH$dt, IV$dt), KNOTES=c(IH$KNOTES, IV$KNOTES), 
    info=ginfo,
    dat=c(IH$dat, IV$dat), nn=c(IH$nn, IV$nn), ex=IH$ex, pcol=c(IH$pcol, IV$pcol), ok=IH$ok, wintim=IH$wintim,  ftime=IH$ftime )

  invisible(GFIL)


}



`BKpfile2ypx` <-
function(P)
{
  #############  read in output from the ttimes program of Brian Kennett
  #######



nam = attr(P, "name")
LOC = attr(P, "LOC")
  

  
YPX=list(tag="",
      name="",
      comp="",
      c3="",
      phase="",
      err=0,
      pol=0,
      flg=0,
      res=0,
      yr = 0,
      mo = 0,
      dom = 0,
      jd =0,
      hr=0,
      mi = 0,
      sec =0,
      col = "black",
      onoff =0
      )


  n = length(P$sec)



YPX$name=rep(nam, length=n)
YPX$comp=rep("V", length=n)
YPX$tag=rep(paste(sep=".", nam, "V")  , length=n)
YPX$c3=rep(nam, length=n)

  YPX$yr = rep(LOC$yr, length=n)
  YPX$mo = rep(LOC$mo, length=n)
  YPX$dom = rep(LOC$dom, length=n)
  YPX$jd = rep(LOC$jd, length=n)
  YPX$hr = rep(LOC$hr, length=n)
  YPX$mi = rep(LOC$mi, length=n)
  YPX$sec = P$sec
  YPX$phase = P$phase
  pcol=rep("springgreen4", length(YPX$sec))
  phas = YPX$phase
  pcol[phas=="P"] = "violetred2"
  pcol[phas=="S"] = "deepskyblue4"
  
  YPX$col = pcol
  YPX$onoff = rep(0, length(YPX$sec))

  YPX$err= rep(0, length(YPX$sec))
  YPX$pol= rep(0, length(YPX$sec))
  YPX$flg= rep(0, length(YPX$sec))
  YPX$res= rep(0, length(YPX$sec))

YPX$cat=YPX$phase

  return(YPX)

}


pickhandler<-function(i1=1, ppick=0, kzap="Y", err=NA, ycol=rgb(0,0,1) , NPX=1, WPX=WPX, NH)
  {
#######   used in PICK.GEN for handling picks
    if(missing(i1)) i1=1
    if(missing(kzap)) kzap="Y"
    if(missing(err)) err=NA
     if(missing(ycol)) ycol=rgb(0,0,1)
    if(missing(NPX)) NPX=1
    if(missing(WPX))
      {
        WPX = list(
          tag="",
          name="",
          comp="",
          c3="",
          phase="",
          err=0,
          pol=0,
          flg=0,
          res=0,
          yr=0,
          mo=0,
          dom=0,
          jd=0,
          hr=0,
          mi=0,
          sec=0,
          col='red',
          onoff =0  )
        WPX = data.frame(WPX, stringsAsFactors = FALSE)
      }
    
    ## print(i1)
    asec = NH$info$sec[i1]+NH$info$msec[i1]/1000+NH$info$t1[i1]-NH$info$off[i1]+ppick
    pic1 = recdate(NH$info$jd[i1], NH$info$hr[i1], NH$info$mi[i1], asec)

    WPX$tag[NPX]=paste(sep=".",NH$STNS[i1],  NH$COMPS[i1])
    WPX$name[NPX]=NH$STNS[i1]
    WPX$comp[NPX]=NH$COMPS[i1]
    WPX$c3[NPX]=NH$OCOMPS[i1]
    WPX$phase[NPX]=kzap
    
    WPX$err[NPX]=err
    WPX$pol[NPX]=0
    WPX$flg[NPX]=0
    WPX$res[NPX]=NA
    WPX$yr[NPX]=NH$info$yr[i1]
    WPX$mo[NPX]= NH$info$mo[i1]
    WPX$dom[NPX]=NH$info$dom[i1]
    WPX$jd[NPX]=pic1$jd
    WPX$hr[NPX]=pic1$hr
    WPX$mi[NPX]=pic1$mi
    WPX$sec[NPX]=pic1$sec
    WPX$col[NPX]=ycol
    WPX$onoff[NPX] = 1

    return(WPX)
    
  }

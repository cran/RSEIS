###################################
JustF<-function(nh, g)
  {
#####  BUTTONDOC:JustF:'Show only Infrasound'
      kix = legitpix(g$sel, g$zloc, g$zenclick)
      ypick =  kix$ypick
      ppick = kix$ppick

      ord.orig = g$SEL.ORIG
      STA.order = nh$STNS[ord.orig]
      
      sel = which(nh$COMPS=="F")
      msta = match(nh$STNS[sel], STA.order )
        
        jsel =  sel[order(msta) ]
        
      isel = jsel[1]
    
          Torigin = list(jd=nh$info$jd[isel], hr=nh$info$hr[isel],
            mi=nh$info$mi[isel],
            sec=(nh$info$sec[isel]+nh$info$msec[isel]/1000+nh$info$t1[isel]-nh$info$off[isel]))
       g$Torigin=Torigin
          g$sel = jsel

    g$zloc = list(x=NULL, y=NULL)
    g$STNS = nh$STNS[jsel]
    g$COMPS = nh$COMPS[jsel]
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
  }


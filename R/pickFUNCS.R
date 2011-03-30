YPIX<-function(nh, g)
  {
    if(g$zenclick>=2)
      {
        zappa = match(g$KLICK, g$BLABS)
        col = g$colpix[which(g$pnos=="YPIX")]
        kix = legitpix(g$sel, g$zloc, g$zenclick)
        ypick =  kix$ypick
        ppick = kix$ppick

############   proceed only if have legitimate picks
        if(length(ypick)>0)
          {
            azap = "YPIX"
            kzap = "Y"
            ipick = g$sel[ypick]
            
#### print(paste(sep=" ", "DUMP YPIX", zappa, col, azap, kzap , ppick , ypick,ipick)) 
            
            for(iz in 1:length(ypick))
              {
                g$NPX = g$NPX+1
                Nn = names(g$WPX)
               g$WPX =rbind(g$WPX, rep(NA, length(Nn)))
                
                i1 = ipick[iz]
                i2 = ypick[iz]

                ycol = g$colpix[zappa]
                if(is.na(ycol)) { ycol = rgb(0,0,1) }
                err = NA
                res = 0
                g$WPX =  pickhandler(i1=i1, ppick=ppick[iz], kzap=kzap, res=res, err=NA, ycol=ycol, onoff=2, NPX=g$NPX, WPX=g$WPX, NH=nh)
               ##  g$ADDPIX =  pickhandler(i1=i1, ppick=ppick[iz], kzap=kzap, res=res, err=NA, ycol=ycol, NPX=g$NPX, WPX=g$WPX, NH=nh)
                g$NADDPIX = g$NADDPIX+1
                
                ## 
              }
###PLOT.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE, FORCE=forcepix, cex=pcex)
            g$PHASE = unique( c(g$PHASE, "Y") )
          }
      }

    ## print(g$PHASE)

    
    g$zloc = list(x=NULL, y=NULL) 
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
  }
############################################################
WPIX<-function(nh, g)
  {
    #####   WPIX are introduced as pairs of pix
    if(g$zenclick>=2)
      {
        zappa = match(g$KLICK, g$BLABS)
        
        kix = legitpix(g$sel, g$zloc, g$zenclick)
        ypick =  kix$ypick
        ppick = kix$ppick
        LIX = floor(length(ypick)/2)

        
############   proceed only if have legitimate picks
        if(LIX>0)
          {
            azap = "WPIX"
            kzap = "W"
            ipick = g$sel[ypick]
            
print(paste(sep=" ", "DUMP WPIX", zappa, azap, kzap , ppick , ypick,ipick)) 
            
            for(LZ in 1:LIX)
              {
                iz = (LZ)*2-1
                
                g$NPX = g$NPX+1
                Nn = names(g$WPX)
               g$WPX =rbind(g$WPX, rep(NA, length(Nn)))
                
                i1 = ipick[iz]
                ycol = g$colpix[zappa]
                if(is.na(ycol)) { ycol = rgb(0,0,1) }
                err = NA
                res = ppick[iz+1]-ppick[iz]
                print(paste(i1, iz, ppick[iz], kzap, res, err, ycol))
                g$WPX =  pickhandler(i1=i1, ppick=ppick[iz], kzap=kzap, res=res, err=err, ycol=ycol, NPX=g$NPX, WPX=g$WPX, NH=nh)
                g$NADDPIX = g$NADDPIX+1
                
                ## 
              }
###PLOT.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE, FORCE=forcepix, cex=pcex)
            
          }
      }
    g$zloc = list(x=NULL, y=NULL) 
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
  }

##########################
NOPIX<-function(nh, g)
  {
    g$WPX$onoff = rep(-1, length(g$WPX$onoff))
    g$zloc = list(x=NULL, y=NULL) 
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
  }
##########################
REPIX<-function(nh, g)
  {
    g$WPX$onoff[g$WPX$onoff==(-1)] = 0
    g$zloc = list(x=NULL, y=NULL) 
   g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
  }
##########################



##########################
FILLPIX<-function(nh, g)
  {
    g$fillpix = !g$fillpix 
    g$zloc = list(x=NULL, y=NULL) 
   g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
  }
##########################

##########################
RIDPIX<-function(nh, g)
  {

    tol = .1
############   find picks near the clicks and remove...temporarily
    if(g$zenclick>=2)
      {
        zappa = match(g$KLICK, g$BLABS)
        col = g$colpix[which(g$pnos=="YPIX")]
        kix = legitpix(g$sel, g$zloc, g$zenclick)
        ypick =  kix$ypick
        ppick = kix$ppick

############   proceed only if have legitimate clicks
        if(length(ypick)>0)
          {
            
            ipick = g$sel[ypick]
            
####
            
            
            for(iz in 1:length(ypick))
              {
                
                i1 = ipick[iz]
                i2 = ypick[iz]
              ##  print(paste(iz, i1, i2))

                
                asec = nh$info$sec[i1]+nh$info$msec[i1]/1000+nh$info$t1[i1]-nh$info$off[i1]+ppick[iz]
                pic1 = recdate(nh$info$jd[i1], nh$info$hr[i1], nh$info$mi[i1], asec)


             ##  print(pic1)
                
                ds = abs(secdifL(g$WPX, pic1))
                

            ##    print(ds)

                if( any(ds<=tol) )
                  {
                    
                    irid = which.min(ds)

                    
                  ##   print(irid)
                    samesta = g$WPX$name[irid]==nh$STNS[i1] & g$WPX$comp[irid]==nh$COMPS[i1]
                    if(samesta) g$WPX$onoff[irid] = -1
                  }

                
                
             ##   g$WPX =  pickhandler(i1=i1, ppick=ppick[iz], kzap=kzap, res=res, err=NA, ycol=ycol, NPX=g$NPX, WPX=g$WPX, NH=nh)
               ##  g$ADDPIX =  pickhandler(i1=i1, ppick=ppick[iz], kzap=kzap, res=res, err=NA, ycol=ycol, NPX=g$NPX, WPX=g$WPX, NH=nh)
               ## g$NADDPIX = g$NADDPIX+1
                
                ## 
              }
###PLOT.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE, FORCE=forcepix, cex=pcex)
            
          }
      }
    g$zloc = list(x=NULL, y=NULL) 
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
    

    
  }
##########################


##########################
SEEPIX<-function(nh, g)
  {
    options(width=180)
    print(g$WPX)
    
    #### write.table(g$WPX)
    g$zloc = list(x=NULL, y=NULL) 
   g$action = "donothing"
    invisible(list(NH=nh, global.vars=g))
  }
##########################




PickWin<-function(nh, g)
  {

     kix = legitpix(g$sel, g$zloc, g$zenclick)
          ypick =  kix$ypick
          ppick = kix$ppick
                
          if(length(ppick)>0)
            {
            
              ipick = g$sel[ypick]

              ipick = ipick[length(ipick)]
              
             ## cat(paste(sep=" ", ypick, ipick), sep="\n")
             ## print(ipick)
              ##
              
              ma = which(!is.na(match( nh$STNS, nh$STNS[ipick])))

              
              ##########   sort so Vertical is on top and then North and East
              acomp  = nh$COMPS[ma]
              icomp = rep(0, length(acomp))
              icomp[acomp=="V"] = 1
              icomp[acomp=="N"] = 2
              icomp[acomp=="E"] = 3

              ma = ma[order(icomp)]

              
####  print(cbind(nh$STNS[ma], nh$COMPS[ma]))

              
              if(is.null(g$Pickdev))
                {
               ####   X11(width = 12, height = 7)
                  screens(2)
                  devl = dev.list()
                  iw =  which(g$MAINdev!=devl)
                  
                  g$Pickdev = devl[iw[1]]
                   dev.set(g$Pickdev)
                }
              else
                {
                 #### devl = dev.list()
                ####  jsc = 2-length(devl)
                ####  if(jsc>0) { X11(width = 12, height = 7); Pickdev = dev.cur() }
                  dev.set(g$Pickdev)
                }

              if(g$zenclick>2)
                {

                  pickwin = range( c(g$zloc$x[(g$zenclick-1)], g$zloc$x[(g$zenclick-2)]))
                  
                }
              else
                {
                  pickwin = g$WIN

                }
              
              PICKLAB = c("DONE", "ZOOM.out","ZOOM.in", "REFRESH", "RESTORE",
                "FILT", "UNFILT", "Pinfo", "WINFO")

              PLAB=c( "Ppic", "Spic", "Apic",  "Pup", "Pdown", "Pnil", "AUTOP",
                "NOPIX", "EDIX", "REPIX")

              stit = nh$STNS[ma[1]]
              ##  SWP = selAPX(WPX,  nh$STNS[ma[1]], icomp=NULL )

              ##   print(data.frame(SWP))
              ##   SWP = rectifyAPX(SWP)
              ##
              ## print(SWP)

              
              newpicks = swig(nh, APIX=g$WPX, sel=ma, WIN=pickwin,
                STDLAB=PICKLAB ,PADDLAB=PLAB, PHASE=1   ,
                SHOWONLY = FALSE, TIT=stit)


              
              if(length(newpicks$g$WPX)>=1)
                {
                  if(!is.null(newpicks$g$WPX))
                    {
                      g$WPX = newpicks$g$WPX
                    }
                }
              ##  
              
              ##
####    print(cbind(WPX$name, WPX$comp, WPX$phase, WPX$onoff))
              g$NPX = length(g$WPX$name)
              print(paste(sep=' ', "DONE with PICKWIN", g$NPX))
              dev.set( g$MAINdev)

              
              
            }
          
          g$zloc = list(x=NULL, y=NULL) 
          

   g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
  }

pADDPIX<-function(nh, g, phase)
  {
   zappa = match(g$KLICK, g$PADDLAB)
    azap = g$PADDLAB[zappa]
    print(paste(sep=" ", "My PICKIN", azap, zappa))

    kix = legitpix(g$sel, g$zloc, g$zenclick)
    ypick =  kix$ypick
    ppick = kix$ppick
    
###   print(paste(sep=" " , "WIN=",sloc$x))
    
###        abline(v=ppick, col=4)
    
    ipick = g$sel[ypick]


   ####################   here I take the first click - but is that right?
   ####  is this because I am forcing only one P-wave arrival?
   ####   that does not make sense.....


   
    ipick = ipick[1]

    
    print(paste(sep=" ", "PICK=", nh$info$yr[ipick], nh$info$jd[ipick], nh$info$hr[ipick],
                nh$info$mi[ipick], "sta=", nh$STNS[ipick], "comp=", nh$COMPS[ipick] ))

    m = match(g$STNS[ypick],g$UNIsta)
###  Upix[[m]]$x  = ppick
    
###   PPIX(list(x=zloc$x[zenclick-1], y=zloc$y[zenclick-1]), YN=NSEL, col=3, lab="P")
    jj = floor((g$zloc$y[g$zenclick-1])/g$du)
    
    if((g$zenclick==2))
      {
        asec = nh$info$sec[ipick]+nh$info$msec[ipick]/1000+
          nh$info$t1[ipick]-nh$info$off[ipick]+ppick[g$zenclick-1]
        err = 0.05
      }
    else
      {
        asec = nh$info$sec[ipick]+nh$info$msec[ipick]/1000+
          nh$info$t1[ipick]-nh$info$off[ipick]+ppick[g$zenclick-2]
        bsec = nh$info$sec[ipick]+nh$info$msec[ipick]/1000+
          nh$info$t1[ipick]-nh$info$off[ipick]+ppick[g$zenclick-1]
        err =  abs(bsec-asec)
      }

###########   this looks like a bug./....
    
    iseek = which(g$WPX$phase==phase & g$WPX$name==nh$STNS[ipick] &  g$WPX$comp==nh$COMPS[ipick])

   
###  print(paste(sep=" ", "ISEEK",  iseek, length(iseek) ))
    
    if(length(iseek)==1)
      {
        wNPX = iseek
        
        g$WPX$yr[wNPX]=nh$info$yr[ipick]
        g$WPX$mo[wNPX]= nh$info$mo[ipick]
        g$WPX$dom[wNPX]=nh$info$dom[ipick]
        g$WPX$jd[wNPX]=nh$info$jd[ipick]
        g$WPX$hr[wNPX]= nh$info$hr[ipick]
        g$WPX$mi[wNPX]=nh$info$mi[ipick]
        g$WPX$col[wNPX]=g$specpix.col[4]
        g$WPX$sec[wNPX]=asec
        g$WPX$err[wNPX]=err
        g$WPX$onoff[wNPX] = 1 
      }
    else
      {
        g$NPX = g$NPX+1
        wNPX  = g$NPX
####  tag = paste(sep=".",nh$STNS[ipick],  nh$OCOMPS[ipick])
####  print(tag)
        Nn = names(g$WPX)
        g$WPX =rbind(g$WPX, rep(NA, length(Nn)))
        
#########   a 
        g$WPX$tag[wNPX]=paste(sep=".",nh$STNS[ipick],  nh$OCOMPS[ipick])
        
        g$WPX$name[wNPX]=nh$STNS[ipick]
        g$WPX$comp[wNPX]=nh$COMPS[ipick]
        g$WPX$c3[wNPX]=nh$OCOMPS[ipick]
        g$WPX$phase[wNPX]=phase
        
        g$WPX$yr[wNPX]=nh$info$yr[ipick]
        g$WPX$mo[wNPX]= nh$info$mo[ipick]
        g$WPX$dom[wNPX]=nh$info$dom[ipick]
        g$WPX$jd[wNPX]=nh$info$jd[ipick]
        g$WPX$hr[wNPX]= nh$info$hr[ipick]
        g$WPX$mi[wNPX]=nh$info$mi[ipick]
        g$WPX$sec[wNPX]=asec
        g$WPX$col[wNPX]=g$specpix.col[4]
        g$WPX$onoff[wNPX] = 1 
        g$WPX$err[wNPX]=err
        g$WPX$flg[wNPX] = 0
        g$WPX$res[wNPX] = NA

      }
    
    
    
    
    g$NADDPIX = 3
###

    
   return(g)

  }


####################################
Ppic<-function(nh, g)
  {

    
    g = pADDPIX(nh, g, "P")
    
    g$zloc = list(x=NULL, y=NULL)
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
    
  }
#####
Spic<-function(nh, g)
  {

    
    g = pADDPIX(nh, g, "S")
    
    g$zloc = list(x=NULL, y=NULL)
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
    
  }
#####
Apic<-function(nh, g)
  {
    g = pADDPIX(nh, g, "A")
    
    g$zloc = list(x=NULL, y=NULL)
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
    
  }
####################################
POLSWITCH<-function(nh, g, dir)
  {

    zappa = match(g$KLICK, g$PADDLAB)
    azap = g$PADDLAB[zappa]
    print(paste(sep=" ", "My PICK", dir, azap, zappa))

    kix = legitpix(g$sel, g$zloc, g$zenclick)
    ypick =  kix$ypick
    ppick = kix$ppick
    
    
    
    ipick = g$sel[ypick]
    
    print(paste(sep=" ", "PICK=", nh$info$yr[ipick], nh$info$jd[ipick], nh$info$hr[ipick],
                nh$info$mi[ipick], "sta=", nh$STNS[ipick], "comp=", nh$COMPS[ipick] ))

    m = match(g$STNS[ypick],g$UNIsta)

    jj = floor((g$zloc$y[g$zenclick-1])/g$du)
    
    iseek = which(g$WPX$phase=="P" & g$WPX$name==nh$STNS[ipick] &  g$WPX$comp==nh$COMPS[ipick])
####  print(paste(sep=" ", "ISEEK",  iseek, length(iseek) ))
    
    if(length(iseek)==1)
      {
        wNPX = iseek
        
        g$WPX$pol[wNPX]=dir
        
      }
    return(g)

  }
#####  polarity determinations
Pup<-function(nh, g)
  {

    g = POLSWITCH(nh, g, "U")
    g$zloc = list(x=NULL, y=NULL)
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
    
  }
#####
Pnil<-function(nh, g)
  {

    g = POLSWITCH(nh, g, "N")
    
    g$zloc = list(x=NULL, y=NULL)
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
    
  }
#####
Pdown<-function(nh, g)
  {

    g = POLSWITCH(nh, g, "D")
    g$zloc = list(x=NULL, y=NULL)
    g$action = "replot"
    invisible(list(NH=nh, global.vars=g))
    
  }
####################################


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



iNEXT<-function(nh, g)
  {
    g$zloc = list(x=NULL, y=NULL)
    g$action = "break"
    invisible(list(NH=nh, global.vars=g))
   
  }
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

###  need here something to scroll through the stations

        usta = unique(nh$STNS)
        uN = length(usta)
        mst = match( nh$STNS, usta)
        cycl = seq(from=1, to=uN)

        
        ksta = which(nh$STNS[ipick] == usta)
        
        
        while(TRUE)
          {
            
            jsta = ((ksta-1) %% uN) + 1
            cat(paste(jsta, ksta), sep="\n" )
            ma = which(!is.na(match( nh$STNS, usta[jsta] )))
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
            
            PICKLAB = c("DONE",  "iNEXT", "ZOOM.out","ZOOM.in", "REFRESH", "RESTORE",
              "FILT", "UNFILT", "Pinfo", "WINFO", "ROT.RT")

            PLAB=c( "Ppic", "Spic",  "Apic",  "Pup", "Pdown", "Pnil", "AUTOP",
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


            print(newpicks$but)
            
            if(length(newpicks$g$WPX)>=1)
              {
                if(!is.null(newpicks$g$WPX))
                  {
                    g$WPX = newpicks$g$WPX
                  }
              }
            if(newpicks$but=="DONE" | newpicks$but=="QUIT"  ) break
            if(newpicks$but=="iNEXT")
              {
                print("pressed iNEXT")
                ksta = ksta + 1

              }

            
          }
        ##  
        
        ##
####    print(cbind(WPX$name, WPX$comp, WPX$phase, WPX$onoff))
        g$NPX = length(g$WPX$name)
####                print(paste(sep=' ', "DONE with PICKWIN", g$NPX))
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
###     print(paste(sep=" ", "My PICKIN", azap, zappa))

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

####  print(paste(sep=" ", phase, nh$STNS[ipick], nh$COMPS[ipick], "ISEEK",  iseek, length(iseek) ))

   onepx = cleanWPX()

   onepx$phase=phase
   
   onepx$yr=nh$info$yr[ipick]
   onepx$mo= nh$info$mo[ipick]
   onepx$dom=nh$info$dom[ipick]
   onepx$jd=nh$info$jd[ipick]
   onepx$hr= nh$info$hr[ipick]
   onepx$mi=nh$info$mi[ipick]
   onepx$col=g$specpix.col[4]
   onepx$sec=asec
   onepx$err=err
   onepx$onoff = 1
   
   
   if(length(iseek)==1)
     {
##############   replace the pick with the current pick
       wNPX = iseek
       onepx$tag = g$WPX$tag[wNPX]
       onepx$name = g$WPX$name[wNPX]
       onepx$comp = g$WPX$comp[wNPX]
       onepx$c3 = g$WPX$c3[wNPX]

       g$WPX =  replaceWPX(g$WPX, onepx, wNPX)
       
     }
   else
     {
       onepx$name=nh$STNS[ipick]
       onepx$comp=nh$COMPS[ipick]
       onepx$c3=nh$OCOMPS[ipick]
       onepx$tag=paste(sep=".",nh$STNS[ipick],  nh$OCOMPS[ipick])
###############   add a new pick to WPX list
       g$WPX = catWPX(g$WPX,onepx )
       }


   
   g$NPX = length(g$WPX$sec)
             
   g$PHASE = unique( c(g$PHASE, "Y") )
   
   
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
###     print(paste(sep=" ", "My PICK", dir, azap, zappa))

    kix = legitpix(g$sel, g$zloc, g$zenclick)
    ypick =  kix$ypick
    ppick = kix$ppick
    
    
    
    ipick = g$sel[ypick]

    ##   print(ipick)
    
    if(length(ipick)<1)
      {
        ipick = g$sel[which(nh$COMPS[g$sel]=="V" )]
      }
    ##   print(g$sel)
   ##  print(nh$STNS)
   ##  print(nh$COMPS)
    
    print(paste(sep=" ", "PICK=", nh$info$yr[ipick],
                nh$info$jd[ipick], nh$info$hr[ipick],
                nh$info$mi[ipick], "sta=", nh$STNS[ipick],
                "comp=", nh$COMPS[ipick] ))

    m = match(g$STNS[ypick],g$UNIsta)

    jj = floor((g$zloc$y[g$zenclick-1])/g$du)
    
    iseek = which(g$WPX$phase=="P" & g$WPX$name==nh$STNS[ipick] &  g$WPX$comp==nh$COMPS[ipick])
####  print(paste(sep=" ", "ISEEK",  iseek, length(iseek) ))
    
    if(length(iseek)==1)
      {
        wNPX = iseek
        
        g$WPX$pol[wNPX]=dir
        
      }
    else
      {
        print(paste(sep=" ", "NO MATCH FOUND ISEEK",  iseek, length(iseek) ))
        print("Click in a panel first, then select polarity")

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

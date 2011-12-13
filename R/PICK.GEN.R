`PICK.GEN` <-function(GH, sel=1:length(GH$dt), ORD=NULL, WIN=NULL, APIX=NULL, PHASE=NULL,  STDLAB=NULL,
                   PADDLAB=NULL, TEMPBUT=NULL, SHOWONLY=FALSE, CHOP=FALSE, TIT="", pts=FALSE, forcepix=FALSE, pcex=0.7, SCALE=1,
         velfile="", stafile="", LOC=NULL, FILT=list(fl=.2, fh=15,  type="HP", proto="BU"), filters=NULL  )
{
###  a = PICK.MARIO(GH,  sel, WIN=twin)
  #########  velfile is a 1D velocity file
  #########  stafile is a file station locations
  
  if(missing(WIN)) { WIN = NULL }
  if(missing(sel)) { sel = 1:length(GH$dt)}
   if(missing(ORD)) { ORD = NULL } 
 ###   if(missing(APIX)) { APIX = NULL}  else { if(!exists(deparse(substitute(APIX)))) { print("WARNING: NO WPX"); APIX=NULL} }

  if(missing(APIX)) { APIX = NULL}
  if(missing(PHASE)) { PHASE = NULL}
  
  if(missing(SHOWONLY)) { SHOWONLY=FALSE}
  if(missing(CHOP)) { CHOP=FALSE }
   if(missing(STDLAB)) { STDLAB = c("DONE", "QUIT","PSEL","zoom out", "zoom in", "Left", "Right", "restore", "Pinfo","WINFO",
                           "XTR", "SPEC", "SGRAM" ,"WLET", "FILT", "UNFILT", "SCALE", "Postscript")}
  if(missing(PADDLAB)) { PADDLAB=c( "NOPIX", "REPIX") }

  if(missing(TEMPBUT)) { TEMPBUT=NULL } 
  
  if(missing(TIT)) { TIT=NULL }
  if(missing(pts)) { pts=FALSE }
  if(missing(forcepix)) { forcepix=FALSE }
 if(missing(pcex)) { pcex = 0.7 }
    
  if(missing(velfile)) {
    if(!is.null(GH$velfile)) {velfile=GH$velfile } else { 
    velfile=NULL }
  }
  
  if(missing(stafile)) {
     if(!is.null(GH$stafile)) {stafile=GH$stafile } else {  
    stafile=NULL }
  }

  
  if(missing(LOC)) { LOC=NULL }

  if(missing(SCALE)) {  ScaleFACT = 1 } else {  ScaleFACT = SCALE }
  if(missing(FILT)) {  FILT = list(fl=.2, fh=15,  type="HP", proto="BU") }

  if(missing(filters)) { filters = NULL }

if(is.logical(sel)) { sel = which(sel) } 
  
 if( is.null(sel) ) { sel = 1:length(GH$dt) }


   legitpix<-function(sel, zloc, zenclick)
    {
      legpick = length(sel)-floor(length(sel)*zloc$y[1:(zenclick-1)])
      ileg = which(legpick>=1 & legpick<=length(sel))
      
      ypick  = legpick[ ileg ]
      ppick  = zloc$x[ ileg ]
     
      return( list(ypick=ypick, ppick=ppick) )
      
    }

  getrdpix<-function(zloc,zenclick,sel,NH   )
    {

      
      kix = legitpix(sel, zloc, zenclick)

     ##  print(kix$ypick)
      
      if(length(kix$ypick)<1) {

        ## print("No legit picks")
        return(NULL)


      }
      
      ypick =  kix$ypick
      ppick = kix$ppick
      
      ipick = sel[ypick]
      
      asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick
      
      rd = recdate( NH$info$jd[ipick], NH$info$hr[ipick], NH$info$mi[ipick], asec, yr=NH$info$yr[ipick])
      
      
      rd$stn =  NH$STNS[ipick]
      rd$comp = NH$COMPS[ipick]
      invisible(rd) 
    }
  

  YNreplot<-function()
    {
      
      YN = PLOT.SEISN(NH, WIN=WIN, dt=NH$dt[sel], sel=sel, sfact=ScaleFACT , notes=NH$KNOTES[sel], COL=pcols, TIT=TIT, SHIFT=ASHIFT , pts=pts)

      YN$STNS = NH$STNS[sel]
      YN$COMPS = NH$COMPS[sel]
      YN$notes = NH$KNOTES[sel]
      
      if(NPX>0)
        {
          PLOT.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE, FORCE=forcepix, cex=pcex)
##### PLOT.WPX(Torigin, STNS, COMPS, WPX, FORCE=forcepix)
        }
      invisible(YN)
    }
  
  
  SEL.ORIG = sel

  SHIFT.ORIG = NULL
  ASHIFT = SHIFT.ORIG  
  
  mark = FALSE


###################   copy of GH file ..... do I need this?
  if(CHOP==TRUE)
    {
      if(!is.null(WIN))
        {
          NH = CHOP.SEISN(GH, sel , WIN=WIN)
        }
      else
        {
          
          NH = GH
        }
      
      WIN = c(0, NH$dt*length(NH$JSTR[[1]]))
      
    }
  else
    {
      
      NH = GH
    }


  if( identical(is.na(match("NOPIX",  PADDLAB)), TRUE)) { PADDLAB = c(PADDLAB, "NOPIX") }
  if( identical(is.na(match("REPIX",  PADDLAB)), TRUE)) { PADDLAB = c(PADDLAB, "REPIX") }

  if(!exists('STDLAB'))
    {
      STDLAB = c("DONE",  "zoom out", "refresh", "restore", "SavePF", 
        "PickWin", "XTR", "SPEC", "SGRAM" ,"WLET")
    }

  stdlab =  STDLAB

  Pickdev = NULL
  Mapdev = NULL

  tempbuttons = NULL

  BLABS = c(stdlab, PADDLAB)
  NLABS = length(BLABS)
  NOLAB = NLABS +1000
  
  ##  match("", BLABS)

  RETX =  NULL
somecolors = c("black", "darkmagenta", "forestgreen", "blueviolet",
        "tan3", "lightseagreen", "deeppink", "cyan3", "bisque3",
        "magenta1", "lightsalmon3", "darkcyan", "darkslateblue",
        "chocolate4", "goldenrod4", "mediumseagreen")

APAL=c("black","darkmagenta","forestgreen","blueviolet",
  "tan3","lightseagreen","deeppink","cyan3","bisque3",
  "darkcyan","darkred","firebrick3","rosybrown4","royalblue3",
  "darkblue","red2","violetred3","springgreen3","darkorange4",
  "palevioletred3","mediumpurple3","tomato","dodgerblue1",
  "olivedrab4","yellow4","pink4")

##   pnos = grep("PIX", BLABS)
   pnos = c( grep("PIX", BLABS), grep("pik", BLABS))
  colabs = rep(1,length(BLABS))
  colabs[BLABS=="PickWin"] = 'red'
  colabs[pnos] = somecolors[seq(from=2, length=length(pnos))]
  
  colpix = somecolors[seq(from=2, length=length(pnos))]
  
  pchlabs = rep(4,length(BLABS))
  pchlabs[pnos] = seq(from=15, length=length(pnos))

  specpix =     c("P", "S", "A", "P1")
  specpix.col = c("violetred", "darkgoldenrod", "blueviolet" , "darkmagenta")
  
  
  NSEL = length(NH$dt[sel])

  if(is.null(APIX)==TRUE)
    {
      WPX = list(
        tag=NA,
        name=NA,
        comp=NA,
        c3=NA,
        phase=NA,
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
        col='red',  onoff =0  )
       WPX = data.frame(WPX, stringsAsFactors = FALSE)
      NPX = 0
    }
  else
    {
      ## print("reading in pickfile")
      ##  
      WPX = APIX
      WPX = data.frame(WPX, stringsAsFactors = FALSE)
      NPX = length(WPX$sec)

      ##  print(paste(sep=' ', "read in pickfile",NPX))
      ## print(xpix)
    }
  
   RIDPIX = list()
   ADDPIX = list()
   NADDPIX = 0
 
  BRUNKOUNT = 0
  BRUNINFO = list()

  DETLKOUNT = 0
  DETLINFO = list()

  
  STNS = NH$STNS[sel]
  COMPS = NH$COMPS[sel]

   ###   print(STNS)
  ###   print(COMPS)
 
  UNIsta = unique(STNS)
  
  NUNI = length( UNIsta)

  if( identical(NH$pcol , "AUTO") |  is.null( NH$pcol )  )
    {

      
      pcols = rep(rgb(0,0,0), length(NH$dt) )
      pcols[c(grep("1", COMPS), grep("I", COMPS), grep("LD", COMPS) )] = rgb(0,.4,0)
      pcols[c(grep("4", COMPS), grep("V", COMPS), grep("Z", COMPS), grep("v", COMPS), grep("z", COMPS)   )] = rgb(0.4,0,0)
      
      pcols[c(grep("J", COMPS), grep("K", COMPS))] = rgb(0,0,0.4)
      
    }
  else
    {

     pcols = NH$pcol

     if(is.numeric(pcols))
       {
     pcols = APAL[1+((pcols-1) %% length(APAL))]
        

       }

    }

  ###  print(pcols)

###   want the sorting of comps to be Vertical North East always

  oCOMPS = COMPS

  oCOMPS[grep("LD", COMPS)] = 1  
  oCOMPS[grep("I", COMPS)] = 1
  oCOMPS[grep("A", COMPS)] = 1
  
  oCOMPS[grep("V", COMPS)] = 2
  oCOMPS[grep("Z", COMPS)] = 2
  
  oCOMPS[grep("v", COMPS)] = 2
  oCOMPS[grep("z", COMPS)] = 2

  oCOMPS[grep("N", COMPS)] = 3
  oCOMPS[grep("n", COMPS)] = 3

  oCOMPS[grep("E", COMPS)] = 4
  oCOMPS[grep("e", COMPS)] = 4


  ords = match(STNS, sort(UNIsta))
  ordc = match(oCOMPS, sort(unique(oCOMPS) ))

  ordsel = order( ords+ordc/10)

    if(!is.null(ORD))
    {
      STNS = STNS[ORD]
      COMPS = COMPS[ORD]
    }

##   print("*************    check stations and comps****** ")
##  print(STNS)
##  print(COMPS)
 
  
  du = 1/NSEL

  ###  pix label size
  
  isel = sel[1]
  
  Torigin = list(jd=NH$info$jd[isel], hr=NH$info$hr[isel], mi=NH$info$mi[isel], sec=(NH$info$sec[isel]+NH$info$msec[isel]/1000+NH$info$t1[isel]-NH$info$off[isel]))

###  print(Torigin)
  
###  print(sel)
###  print(STNS)
###  print(COMPS)
### print(NH$KNOTES[sel])
###  print(NSEL)
###  print(NH$dt[sel])
###  print(pcols[sel])
  LASTwin = WIN
  
###  
  
  YN = YNreplot()

  if(is.numeric(SHOWONLY)) {

    
    Sys.sleep(SHOWONLY);
    return(list(but=NULL, zloc=0, pix=0, YN=YN))

  }
  if(SHOWONLY==TRUE) {

    
    return(list(but=NULL, zloc=0, pix=0, YN=YN))

  }

  
  buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)

  MAINdev = dev.cur()


  ###  Get.Screens(2)
  dev.set( MAINdev)
  
  u = par("usr")
  sloc = list(x=c(u[1],u[2]), y=c(u[3],u[4]))
  zloc =list(x=NULL, y=NULL)
  ppick  = NA
  spick  = NA
  xpick = NA

 zenclick = length(zloc$x)
  
  while(TRUE)
    {
      iloc = ilocator(1, COL=rgb(1,0.6, 0.6), NUM=FALSE , YN=length(sel), style=1)
      Nclick = length(iloc$x)
####  cat(paste(sep=" ", zenclick, Nclick), sep="\n")
      
      if(Nclick>0)
        {
          zloc  = list(x=c(zloc$x,iloc$x), y=c(zloc$y, iloc$y))
          zenclick = length(zloc$x)
          K =  whichbutt(iloc ,buttons)
          sloc = zloc
          
          
        }
      else
        {
###  Right button was clicked
          Nclick = 0
###  zenclick=zenclick+1
###   print(zenclick)
          K = 0
          zenclick = length(zloc$x)
          if(zenclick<1)
            {
              buttons = rowBUTTONS(BLABS, col=rep(grey(.8), length(BLABS)), pch=rep("NULL", length(BLABS)))
              title("Done, Return to Calling Program")
              
              return(list(but="None", zloc=0, pix=0))
            }
          if(zenclick==1)
            {

              WIN = NULL
              
              YN = YNreplot()
             
              buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              u = par("usr")
              
              sloc = list(x=c(u[1],u[2]), y=c(u[3],u[4]))
              
              Nclick=1
              K = 0
              zloc = list(x=NULL, y=NULL)
              zenclick = 0
              
            }
          if(zenclick>=2)
            {
              WIN  = sort(zloc$x[c(zenclick-1, zenclick)])
             YN = YNreplot()
             
              buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              
              Nclick=1
              K = 0
              zloc = list(x=NULL, y=NULL)
              zenclick = 0
            }

        }
     
###  print(paste(sep=' ',  Nclick , zenclick) )
############   button actions
      if(K[Nclick] == match("DONE", BLABS, nomatch = NOLAB))
        {
            buttons = rowBUTTONS(BLABS, col=rep(grey(.8), length(BLABS)), pch=rep("NULL", length(BLABS)))
            title("Return to Calling Program")

            if(zenclick>1)
              {
                rd = getrdpix(zloc, zenclick, sel, NH)
              }
            else
              {
                rd=list(PUSHED="DONE")
              }


          
            
          invisible(list(but=BLABS[K[Nclick]], zloc=zloc, pix=rd ))
          break;
        }

     if(K[Nclick] == match("QUIT", BLABS, nomatch = NOLAB))
        {
           buttons = rowBUTTONS(BLABS, col=rep(grey(.8), length(BLABS)), pch=rep("NULL", length(BLABS)))
            title("Return to Calling Program")


           if(zenclick>1)
             {
               rd = getrdpix(zloc, zenclick, sel, NH)
             }
           else
             {
               rd=list(PUSHED="QUIT")
             }
           
         
          invisible(list(but=BLABS[K[Nclick]], zloc=zloc, pix=rd))
           break;
        }
     if(K[Nclick] == match("NEXT", BLABS, nomatch = NOLAB))
        {
           buttons = rowBUTTONS(BLABS, col=rep(grey(.8), length(BLABS)), pch=rep("NULL", length(BLABS)))
            title("Return to Calling Program")
         if(zenclick>1)
             {
               rd = getrdpix(zloc, zenclick, sel, NH)
             }
           else
             {
               rd=list(PUSHED="NEXT")
             }
           
        
          invisible(list(but=BLABS[K[Nclick]], zloc=zloc, pix=rd))
          break;
          
        }
     if(K[Nclick] == match("PREV", BLABS, nomatch = NOLAB))
        {
           buttons = rowBUTTONS(BLABS, col=rep(grey(.8), length(BLABS)), pch=rep("NULL", length(BLABS)))
            title("Return to Calling Program")

           if(zenclick>1)
             {
               rd = getrdpix(zloc, zenclick, sel, NH)
             }
           else
             {
               rd=list(PUSHED="PREV")
             }
           
          invisible(list(but=BLABS[K[Nclick]], zloc=zloc, pix=rd))
           break;
        }
     if(K[Nclick] == match("HALF", BLABS, nomatch = NOLAB))
        {
           buttons = rowBUTTONS(BLABS, col=rep(grey(.8), length(BLABS)), pch=rep("NULL", length(BLABS)))
            title("Return to Calling Program")
                 if(zenclick>1)
             {
               rd = getrdpix(zloc, zenclick, sel, NH)
             }
           else
             {
               rd=list(PUSHED="HALF")
             }
           
          
          invisible(list(but=BLABS[K[Nclick]], zloc=zloc, pix=rd))
           break;
        }

      if(K[Nclick] == match("MARK", BLABS, nomatch = NOLAB))
        {
          buttons = rowBUTTONS(BLABS, col=rep(grey(.8), length(BLABS)), pch=rep("NULL", length(BLABS)))
          title("Return to Calling Program")
             if(zenclick>1)
             {
               rd = getrdpix(zloc, zenclick, sel, NH)
             }
           else
             {
               rd=list(PUSHED="MARK")
             }
           
         
          return(list(but=BLABS[K[Nclick]], zloc=zloc, pix=rd))
         break;
          
        }
      
      if(K[Nclick] == match("DOC", BLABS, nomatch = NOLAB))
        {
          
           PICK.DOC(BLABS)
            zloc = list(x=NULL, y=NULL)
        }
      
     #############  two window clicks and a middle mouse click:
        ####################  START  BUTTON DEFINITIONS    ###########################
             ###################   RESTORE  ###########################      
      if(K[Nclick]==match("restore", BLABS, nomatch = NOLAB))
        {
          WIN = NULL

          YN = YNreplot()
        
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
          u = par("usr")
          L = length(sloc$x)
          if(L>1)
            {
              abline(v=sloc$x[c(L-1,L)], col=gray(0.8), lty=2)
            }
          sloc = list(x=c(u[1],u[2]), y=c(u[3],u[4]))
           zloc = list(x=NULL, y=NULL)
        }

       ###################   REfresh  ###########################      
      if(K[Nclick]==match("refresh", BLABS, nomatch = NOLAB))
        {
          YN = YNreplot()
        
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
          u = par("usr")
          L = length(sloc$x)
          if(L>1)
            {
              abline(v=sloc$x[c(L-1,L)], col=gray(0.8), lty=2)
            }
          sloc = list(x=c(u[1],u[2]), y=c(u[3],u[4]))
             zloc = list(x=NULL, y=NULL)
          
        }

      ###################   ZOOM  OUT  ###########################      
      if(K[Nclick]==match("zoom out", BLABS, nomatch = NOLAB))
        {
          u = par("usr")
          DX = (u[2]-u[1])*0.3
          zloc = list(x= c(u[1]-DX, u[2]+DX))
          WIN  = zloc$x
          YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
         #### ftime = Zdate(NH$info, sel[1], WIN[1])
         ####  mtext( ftime, side = 3, at = WIN[1], line=0.5, adj=0)

          sloc = zloc
             zloc = list(x=NULL, y=NULL)
        }
      ###################   ZOOM IN   ###########################      
       if(K[Nclick]==match("zoom in", BLABS, nomatch = NOLAB))
        {

          if(zenclick>=3)
            {

              pwin = range(zloc$x[1:(length(zloc$x)-1)])
              WIN = pwin
            }
          else
            {
              u = par("usr")
              DX = (u[2]-u[1])*0.3
####  zloc = list(x= c(u[1]+DX, u[2]-DX))
              WIN  =list(x= c(u[1]+DX, u[2]-DX))
            }

          YN = YNreplot()
         
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
         #### ftime = Zdate(NH$info, sel[1], WIN[1])
         ####  mtext( ftime, side = 3, at = WIN[1], line=0.5, adj=0)

          sloc = zloc
             zloc = list(x=NULL, y=NULL)
        }
      ###################   Shift Left   ###########################      
       if(K[Nclick]==match("Left", BLABS, nomatch = NOLAB))
        {
          u = par("usr")
          DX = (u[2]-u[1])*0.3
        ####  zloc = list(x= c(u[1]+DX, u[2]+DX))
          WIN  =list(x= c(u[1]+DX, u[2]+DX))
         YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
         #### ftime = Zdate(NH$info, sel[1], WIN[1])
         ####  mtext( ftime, side = 3, at = WIN[1], line=0.5, adj=0)

          sloc = zloc
             zloc = list(x=NULL, y=NULL)
        }
      ###################   Shift Right   ###########################      
   if(K[Nclick]==match("Right", BLABS, nomatch = NOLAB))
        {
          u = par("usr")
          DX = (u[2]-u[1])*0.3
        ###  zloc = list(x= c(u[1]-DX, u[2]-DX))
          WIN  =list(x= c(u[1]-DX, u[2]-DX))
         YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
         #### ftime = Zdate(NH$info, sel[1], WIN[1])
         ####  mtext( ftime, side = 3, at = WIN[1], line=0.5, adj=0)

          sloc = zloc
             zloc = list(x=NULL, y=NULL)
        }



      
      ###################   ZOOM IN   ###########################      
   if(K[Nclick]==match("SCALE", BLABS, nomatch = NOLAB))
        {

          if(ScaleFACT==1)
            {

              ScaleFACT=2
            }
          else
            {
              ScaleFACT=1

            }

          YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
         #### ftime = Zdate(NH$info, sel[1], WIN[1])
         ####  mtext( ftime, side = 3, at = WIN[1], line=0.5, adj=0)

             zloc = list(x=NULL, y=NULL)
        }

      ###################   ZOOM IN   ###########################      
   if(K[Nclick]==match("ZERO", BLABS, nomatch = NOLAB))
        {
             zloc = list(x=NULL, y=NULL)
        }
###################   button to save in file the names of special files (either to reject or save )
   if(K[Nclick]==match("PSEL", BLABS, nomatch = NOLAB))
        {
###################   select subset of traces

          cat("modify selection:", sep="\n" )
          ANSWER=readline("Type in index of traces? ")
          
          sel = ANSWER
          #####     sel = 1:length(NH$info$jd)
           NSEL = length(NH$dt[sel])

          du = 1/NSEL
          
          
          isel = sel[1]
          
          Torigin = list(jd=NH$info$jd[isel],
            hr=NH$info$hr[isel], mi=NH$info$mi[isel],
            sec=(NH$info$sec[isel]+NH$info$msec[isel]/1000+NH$info$t1[isel]-NH$info$off[isel]))

           YN = YNreplot()
  
   
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
             zloc = list(x=NULL, y=NULL)
          
          
        }


   if(K[Nclick]==match("SHOWALL", BLABS, nomatch = NOLAB))
        {
          
          sel = 1:length(NH$info$jd)
           NSEL = length(NH$dt[sel])

          du = 1/NSEL
          
          
          isel = sel[1]
          
          Torigin = list(jd=NH$info$jd[isel], hr=NH$info$hr[isel], mi=NH$info$mi[isel], sec=(NH$info$sec[isel]+NH$info$msec[isel]/1000+NH$info$t1[isel]-NH$info$off[isel]))

           YN = YNreplot()
  
   
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
             zloc = list(x=NULL, y=NULL)
          
          
        }

      if(K[Nclick]==match("SHOWSEL", BLABS, nomatch = NOLAB))
        {
          
          sel = SEL.ORIG
          NSEL = length(NH$dt[sel])

          du = 1/NSEL
          
          
          isel = sel[1]
          
          Torigin = list(jd=NH$info$jd[isel], hr=NH$info$hr[isel], mi=NH$info$mi[isel], sec=(NH$info$sec[isel]+NH$info$msec[isel]/1000+NH$info$t1[isel]-NH$info$off[isel]))

           YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
             zloc = list(x=NULL, y=NULL)
          
          
        }


      
     	if(K[Nclick]==match("saveFN", BLABS, nomatch = NOLAB))
        {
          	
	nowdate = format(Sys.time(), "%Y_%m_%d")

	ampfn = paste(sep=".", 	nowdate, "SAVEFN")

	 CAPP = file.exists(ampfn)	
         cat( NH$info$fn[1]  , file=ampfn,sep="\n", append=CAPP)
   zloc = list(x=NULL, y=NULL)
        }

   
###################   FLIP polarity of trace  ###########################      
 
      if(K[Nclick]==match("FLIP", BLABS, nomatch = NOLAB))
        {
          zenclick = length(zloc$x)

          if(zenclick>1)
            {
              nc = 1:(zenclick-1)
              lnc = length(nc)
              
              ypick = length(sel)-floor(length(sel)*zloc$y[nc])
              ipick = unique( sel[ypick] )

              cat("FLIP: PICK.GEN POLARITY REVERSED: "); cat(ipick, sep=" " ); cat("\n")
              
              for(JJ in 1:length(ipick) )
                {
                  jtr  = ipick[JJ]
                  NH$JSTR[[jtr]] = (-1)*NH$JSTR[[jtr]]
                }
      

          YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
                  }
          else
            {
              cat("FLIP: No traces selected: Try Again"); cat("\n")

            }

          
          zloc = list(x=NULL, y=NULL)
          
        }
###################  Trace Info output   ###########################  

        if(K[Nclick]==match("TR_INFO", BLABS, nomatch = NOLAB))
        {
          
     	for(ipick in 1:length(NH$info$name))

	{
	     print(paste(sep=" ", NH$info$name[ipick], NH$info$t1[ipick], NH$info$dt[ipick], NH$info$n1[ipick]))
	}
        zloc = list(x=NULL, y=NULL)
     	   }


      
       ###################   Postscript output   ###########################  
      if(K[Nclick] == match("Postscript", BLABS, nomatch = NOLAB))
        {
          print(paste(sep=' ' ,"Start postscript PLOT.SEISN"))
          jdev = dev.cur()
          plfname = local.file("pick_gen","eps")
          
          ### postscript(file=plfname, horizontal=TRUE, print.it=FALSE,  onefile=FALSE)
           P = round(par('pin'))

          opar = par(no.readonly = TRUE) 

           postscript(file=plfname , width=P[1], height=P[2], paper = "special", bg=opar$bg, fg=opar$fg, horizontal=FALSE, onefile=TRUE,print.it=FALSE)

          ### par(OPAR)
          print(paste(sep=' ' ,"Doing postscript", plfname))
           YN = YNreplot()
          


 
          if(NPX>0)
            {

              PLOT.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE, FORCE=forcepix, cex=pcex)
             ##### PLOT.WPX(Torigin, STNS, COMPS, WPX, FORCE=forcepix)
            }
          
          
          print(paste(sep=' ' ,"Done creating postscript file: ", plfname))
          dev.off()
          dev.set(jdev)
             zloc = list(x=NULL, y=NULL)
        }


       ###################   PNG output   ###########################  
      if(K[Nclick] == match("PNG", BLABS, nomatch = NOLAB))
        {
          print(paste(sep=' ' ,"Start PNG PLOT.SEISN"))
          jdev = dev.cur()
          plfname = local.file("pick_gen","png")
          
          ### 
          opar = par(no.readonly = TRUE)

       
          Pin = round(par('pin'))
          PIXX =  Pin[1]*72
          PIXY = round(Pin[2]*72)
           png(file=plfname ,
         width = PIXX, height = PIXY, units = "px",
         pointsize = 12, bg = opar$bg)

          ### par(OPAR)
          print(paste(sep=' ' ,"Doing png", plfname))
           YN = YNreplot()
         

          if(NPX>0)
            {

              PLOT.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE, FORCE=forcepix, cex=pcex)
             ##### PLOT.WPX(Torigin, STNS, COMPS, WPX, FORCE=forcepix)
            }
          
          
          print(paste(sep=' ' ,"Done creating png  file: ", plfname))
          dev.off()
          dev.set(jdev)
             zloc = list(x=NULL, y=NULL)
        }




   
      ###################   AUTO  PICKs   ###########################      
 
      if(K[Nclick]==match("AUTOP", BLABS, nomatch = NOLAB) & zenclick>=3)
        {
         ###  u = par("usr")

          kix = legitpix(sel, zloc, zenclick)
              ypick =  kix$ypick
              ppick = kix$ppick
              ipick = sel[ypick]
              
        
          print(paste(sep=' ',"start autopick:", ypick, ipick, NH$info$name[ ipick]))
          if(identical(WIN,NULL))
            {
              tim = NH$dt[1]*seq(from=0,to=length(NH$JSTR[[1]])-1)

            }
         ###   amp = NH$JSTR[[j]][NH$ex>sloc$x[1]&NH$ex<sloc$x[2] ]
          
          print(paste(sep=' ',"WINS", YN$DX[1], YN$DX[2], WIN[1], WIN[2]))
          famp = NH$JSTR[[ipick]]

           pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))
          
          ex = NH$ex
          
          Xamp =  famp[ ex > pwin[1] & ex <pwin[2]]

       ###     dev.set(3)
          
       ###   plot.ts(Xamp)
        ###  locator()
        ###  dev.set(2)
          

          
         ### KPIX = autopick(Xamp, which.max(abs(Xamp)) )
         ###  ktarg = which.max(abs(Xamp))
          
         ### ktarg = floor((zloc$x[zenclick-1]-YN$DX[1])/NH$dt[ypick])
         ktarg = which.max(abs(Xamp))

          ###  print(paste(sep=' ',"DUMP",zloc$x[zenclick-1], ktarg,which.max(abs(Xamp))))

          deltat = NH$info$dt[ ipick]
        ###    KPIX = autopick(Xamp, ktarg )

         ### get(getOption("device"))(width=15, height=10)
### get(getOption("device"))()

         ### get(options("device")$device)(width=15, height=10)

         dev.new(width=15, height=10)
           ### dev.new(width=15, height=10)

          
          ###  X11(width=15, height=10)
          ###  plot.ts(ts(Xamp, deltat=deltat))
 
          if(COMPS[ypick] == "I")
            {
              fy = butfilt(Xamp, fl=.5, fh=20, deltat, "HP", "BU" )
             ###  RAT = ratcurve(fy, dt=deltat, fwlen =  75,  bwlen  = 125, PLOT=TRUE)
              RAT = PSTLTcurve(fy, dt=deltat, fwlen=75,  bwlen=125, perc=0.1, stretch=1000 , MED=21, PLOT=TRUE)


              
            }
          else
            {

                  fy = butfilt(Xamp, fl=FILT$fl, fh=FILT$fh, deltat, FILT$type, FILT$proto )
          
              Kaol = length(fy)
              vim = round(pretty(c(100,  Kaol*0.15), n=10))
              
              jout = vector()
              
              for(jarjar in 1:length(vim))
                {
                  fwlen=vim[jarjar];
                  bwlen=vim[jarjar]
                  if(jarjar== round(length(vim)/2)  )
                    {
                  RAT = PSTLTcurve(fy, dt=deltat, fwlen=fwlen,  bwlen=bwlen, perc=0.10, stretch=1000 , MED=21, PLOT=TRUE)
                }
                  else
                    {
                      RAT = PSTLTcurve(fy, dt=deltat, fwlen=fwlen,  bwlen=bwlen, perc=0.10, stretch=1000 , MED=21, PLOT=FALSE)
                    }
                  ##  locator()

                  if(length(RAT$ind)<1)  RAT$ind = NA
                  if(length(RAT$mix)<1)  RAT$mix = NA
                  if(length(RAT$eye)<1)  RAT$eye = NA
                  
                  if(is.na(RAT$ind) | is.null(RAT$ind) ) RAT$ind=NA
                  if(is.na(RAT$eye) | is.null(RAT$eye) ) RAT$eye=NA
                  if(is.na(RAT$mix) | is.null(RAT$mix) ) RAT$mix=NA
                  
                  if(length(c(RAT$ind, RAT$eye, RAT$mix))<3) next
                  jout = rbind(jout, c(RAT$ind, RAT$eye, RAT$mix) )

                  
                }


              if(is.matrix(jout))
                {
                  dj = dim(jout)
                  print(paste("jout=", dj[1], dj[2]))
                  print(jout)
                  RAT$ind =  median(jout[,1], na.rm = TRUE)
                  RAT$eye  = median(jout[,2], na.rm = TRUE)
                  RAT$mix  = median(jout[,3], na.rm = TRUE)
                }
              else
                {
                  print(paste(jout , collapse=" "))
                  RAT$ind =  jout[1]
                  RAT$eye  = jout[2]
                  RAT$mix  = jout[3]
                  
                }
              
              
              if(is.na(RAT$eye) | is.null(RAT$eye) ) RAT$eye=Kaol/2
              
              araict = GETARAIC(fy, DT=deltat, T1=RAT$eye, Mar=8, O1=2, O2=0.2, WW=2,   PLOT=FALSE)
              print(araict)
              
              
              
              
            }
              ##################################################
               ########  
          
          dev.set( MAINdev)
          
          autpix = c(pwin[1]+RAT$ind*NH$dt[ipick], pwin[1]+RAT$eye*NH$dt[ipick], pwin[1]+RAT$mix*NH$dt[ipick], pwin[1]+araict*NH$dt[ipick])
         ###   print(autpix*NH$dt[ipick])
          cls = c(2,3,4, 4)
          plit = !is.na( autpix) & !is.null(autpix)
          abline(v=autpix[plit ]    , col=cls[plit])

        #######  print(paste(sep=" ", "going to vline....", pwin[1], "ara=",araict, "ip=",ipick, NH$dt[ipick]))

          if(length(araict)>0 )
            {
              if(!is.na( araict ) |  !is.null( araict )  )
              vline(pwin[1]+araict*NH$dt[ipick] ,  per =-0.2 , COL=rgb(.9, .5, .5) )
            }
          
             zloc = list(x=NULL, y=NULL)
        }


      ###################   AUTO  PICKs   ###########################      
 
      if(K[Nclick]==match("AUTOPALL", BLABS, nomatch = NOLAB) & zenclick>=3)
        {
         
         ###  u = par("usr")
          kzap = "AP"
          
          ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
        
          
          ipick = sel[ypick]
          print(paste(sep=' ',"start autopick:", ypick, ipick, NH$info$name[ ipick]))
          
         ###   amp = NH$JSTR[[j]][NH$ex>sloc$x[1]&NH$ex<sloc$x[2] ]
          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))
        

          for( ip in 1:length(sel) )
            {
              ipick = sel[ip]
              
              famp = NH$JSTR[[ipick]]

               ###  ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=NH$info$n1[ipick])

              ex = NH$ex
          
              
              Xamp =  famp[ ex > pwin[1] & ex <pwin[2]]

            
### ktarg = floor((zloc$x[zenclick-1]-YN$DX[1])/NH$dt[ypick])
              ktarg = which.max(abs(Xamp))

          ###  print(paste(sep=' ',"DUMP",zloc$x[zenclick-1], ktarg,which.max(abs(Xamp))))

              deltat = NH$info$dt[ ipick]
        ###    KPIX = autopick(Xamp, ktarg )

              Kaol = length(Xamp)
              vim = round(pretty(c(100,  Kaol*0.15), n=10))
              
              jout = vector()
              
              for(jarjar in 1:length(vim))
                {
                  fwlen=vim[jarjar];
                  bwlen=vim[jarjar]
                  if(jarjar== round(length(vim)/2)  )
                    {
                      RAT = PSTLTcurve(Xamp, dt=deltat, fwlen=fwlen,  bwlen=bwlen, perc=0.10, stretch=1000 , MED=21, PLOT=FALSE)
                    }
                  else
                    {
                      RAT = PSTLTcurve(Xamp, dt=deltat, fwlen=fwlen,  bwlen=bwlen, perc=0.10, stretch=1000 , MED=21, PLOT=FALSE)
                    }
                  ##  locator()
                  
                  if(length(RAT$ind)<1)  RAT$ind = NA
                  if(length(RAT$mix)<1)  RAT$mix = NA
                  if(length(RAT$eye)<1)  RAT$eye = NA
                  
                  if(is.na(RAT$ind) | is.null(RAT$ind) ) RAT$ind=NA
                  if(is.na(RAT$eye) | is.null(RAT$eye) ) RAT$eye=NA
                  if(is.na(RAT$mix) | is.null(RAT$mix) ) RAT$mix=NA
                  
                  if(length(c(RAT$ind, RAT$eye, RAT$mix))<3) next
                  jout = rbind(jout, c(RAT$ind, RAT$eye, RAT$mix) )
                  
                  
                }
              
              
              if(is.matrix(jout))
                {
                  dj = dim(jout)
                  print(paste("jout=", dj[1], dj[2]))
                  print(jout)
                  RAT$ind =  median(jout[,1], na.rm = TRUE)
                  RAT$eye  = median(jout[,2], na.rm = TRUE)
                  RAT$mix  = median(jout[,3], na.rm = TRUE)
                }
              else
                {
                  print(paste(jout , collapse=" "))
                  RAT$ind =  jout[1]
                  RAT$eye  = jout[2]
                  RAT$mix  = jout[3]
                  
                }
              
              
              if(is.na(RAT$eye) | is.null(RAT$eye) ) RAT$eye=Kaol/2
              
              araict = GETARAIC(Xamp, DT=deltat, T1=RAT$eye, Mar=8, O1=2, O2=0.2, WW=2,   PLOT=FALSE)


              if(length(araict)>0 )
                {
                  if(!is.na( araict ) |  !is.null( araict )  )
                    {
                      t1 = pwin[1]+araict*NH$dt[ipick]

                      NPX = NPX+1
                      Nn = names(WPX)
                      WPX =rbind(WPX, rep(NA, length(Nn)))
  
                      i1 = ipick
                      
                      asec = NH$info$sec[i1]+NH$info$msec[i1]/1000+NH$info$t1[i1]-NH$info$off[i1]+t1                      
              
                      pic1 = recdate(NH$info$jd[i1], NH$info$hr[i1], NH$info$mi[i1], asec, yr=NH$info$yr[i1])
                      
                      NADDPIX = NADDPIX+1
                      #
                    }
                }
              
              print(araict)
              
            }
                        ##################################################
               ########  source("PICK.R") ; save.image()
       zloc = list(x=NULL, y=NULL)
        }

      ###################   AUTO  PICKs   ###########################      
 
      if(K[Nclick]==match("DETECT", BLABS, nomatch = NOLAB) & zenclick>=3)
        {
         
         ###  u = par("usr")

          
          ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
        
          
          ipick = sel[ypick]
          print(paste(sep=' ',ypick, NH$info$name[ ipick]))
          

          FRWD=20; 
          BKWD=20; 
          sbef = 10; 
          saft = 35; 
          thresh = 1.2; 
          Tthresh2 = 2;  
          flo=1; 
          fhi=10.0
          stretch=1000; 
          PLOT=TRUE; 
          Kmin=10; 
          kind=1
          deltat = 0.008
          DFRWD=.5
          DBKWD=.7


          ZGH = ETECTG(NH, sel=2:3, FRWD =FRWD , BKWD =BKWD , sbef =sbef , saft = saft, DFRWD = DFRWD,
            DBKWD = DBKWD, thresh = thresh, Tthresh2 = Tthresh2 , stretch = stretch, flo =flo ,
            fhi = fhi, PLOT = TRUE, Kmin = Kmin, perc=0.01, kind = 1,  DOARAIC = FALSE)
          
          
          dev.set( MAINdev)
          
          
          ### print(KPIX)
         ### dev.set(dev.next())
          
          ### plot.ts(Xamp)
          ### abline(v=KPIX$ind, col=2)
           ###  dev.set(dev.next())

          autpix = c(pwin[1]+RAT$ind*NH$dt[ipick], pwin[1]+RAT$eye*NH$dt[ipick], pwin[1]+RAT$mix*NH$dt[ipick], pwin[1]+araict*NH$dt[ipick])
          print(autpix*NH$dt[ipick])
          
          abline(v=autpix    , col=c(2,3,4, 4))

        #######  print(paste(sep=" ", "going to vline....", pwin[1], "ara=",araict, "ip=",ipick, NH$dt[ipick]))
          vline(pwin[1]+araict*NH$dt[ipick] ,  per =-0.2 , COL=rgb(.4, 0, 0) )
          
          zloc = list(x=NULL, y=NULL)
        }


      ###################   MAP    ###########################      
 
      if(K[Nclick]==match("MAP", BLABS, nomatch = NOLAB) & Nclick>1)
        {
         ###  u = par("usr")
          Apf =  NH$pickfile
          
          if(!is.null(Apf$STAS$lon))
            {

              ###########   set up stations and source location
              stalats = Apf$STAS$lat
              stalons = Apf$STAS$lon
              stanam  = Apf$STAS$name

              srclat = Apf$LOC$lat
              srclon = Apf$LOC$lon

              print("DOING MAP")
              if(is.null(Mapdev))
                {
                 ### get(getOption("device"))(width = 12, height = 7)
                  dev.new(width = 12, height = 7)
 
                  ###
                 ###  X11(width = 12, height = 7)
                  Mapdev = dev.cur()
                }
              else
                {
                  devl = dev.list()
                  
                  
                  if(is.null(Pickdev))
                    {
                      jsc =  2-length(devl)
                    }
                  else
                    {
                      jsc =  3-length(devl)

                    }

                  
                  if(jsc>0) {
                    ##get(getOption("device"))(width = 12, height = 7)
                    dev.new(width = 12, height = 7)
                    ##X11(width = 12, height = 7);
                    Mapdev = dev.cur()
                            }
                  dev.set(Mapdev)
                }
              ###########  print(pcols)
             
              plot(range( c(stalons,srclon ), na.rm=TRUE),  range( c(stalats,srclat ), na.rm=TRUE),  type='n')
              points(stalons, stalats,  pch=6, col='red')
               points(srclon, srclat,  pch=8, col='blue')

              segments(stalons[sel],  stalats[sel], srclon, srclat , col=pcols[sel])
              text(stalons[sel],  stalats[sel], stanam[sel], pos=3, col=pcols[sel])
              dev.set( MAINdev)
            }

          zloc = list(x=NULL, y=NULL)
        }
      

      
      ###################   XTRACT PART of A trace   ###########################      
 
      if(K[Nclick]==match("XTR", BLABS, nomatch = NOLAB) )
        {
         ###  u = par("usr")

             if(zenclick>=3)
               {
                 
                 ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
                 ipick = sel[ypick]
                 print(paste(sep=' ',"EXTRACT", ypick, NH$info$name[ ipick]))

                 famp = NH$JSTR[[ipick]]

                 pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))

                 ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(famp) )
                 temp =  famp[ ex > pwin[1] & ex <pwin[2]]
                #### Xamp =  -1*temp
                 smallex = ex[ ex > pwin[1] & ex <pwin[2]]

                 asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+pwin[1]
                 
                 spaz = recdate( NH$info$jd[ipick], NH$info$hr[ipick], NH$info$mi[ipick], asec,  NH$info$yr[ipick] )
                 
                 
                 
                 spaz$yr =   as.integer(NH$info$yr[ipick])
              
                 MODAY = getmoday(spaz$jd,  spaz$yr)

                 
                 
                 TP = list(yr=spaz$yr, jd=spaz$jd, mo=MODAY$mo,
                   dom= MODAY$dom  ,hr=spaz$hr, mi=spaz$mi, sec=spaz$sec )


                 RETX = list(but="RET", x=smallex, y=temp, dt=NH$dt[ipick], STNS=NH$STNS[ipick],
                   COMPS=NH$COMPS[ipick],  fname=NH$info$name[ipick] , TIMEpick=TP, mark=TRUE, deltat=NH$dt[ipick] )

                 
                 return(RETX)


               }
             else
               {
               cat("XTR WARNING: no window or trace has been selected:", sep="\n")
             }


             
          zloc = list(x=NULL, y=NULL)
        }
      
      
      ###################   PULSE ANALYSIS   ###########################      
  ######   print(paste(sep=' ',Nclick,  K[Nclick], match("SIG", BLABS)))

      
      if(K[Nclick]==match("SIG", BLABS, nomatch = NOLAB) & zenclick>=3)
        {
         ###  u = par("usr")
          ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
          ipick = sel[ypick]
          print(paste(sep=' ',ypick, NH$info$name[ ipick]))

          famp = NH$JSTR[[ipick]]
          ###  need to flip the accoustic trace?

          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))


          ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(famp) )
          temp =  famp[ ex > pwin[1] & ex <pwin[2]]
          Xamp =  -1*temp
          smallex = ex[ ex > pwin[1] & ex <pwin[2]]

          ##get(getOption("device"))()
          ##
          dev.new()
         ### X11()
          ###  plot.ts(Xamp)

          PULS = tung.pulse( smallex , Xamp , NH$dt[ipick])
          NPX = NPX+1
          Nn = names(WPX)
          WPX =rbind(WPX, rep(NA, length(Nn)))
  
          asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+pwin[1]

          i1 = ipick
                    WPX$tag[NPX]=paste(sep=".",NH$STNS[i1],  NH$COMPS[i1])
                    WPX$name[NPX]=NH$STNS[i1]
                    WPX$comp[NPX]=NH$COMPS[i1]
                    WPX$c3[NPX]=NH$OCOMPS[i1]
                    WPX$phase[NPX]="P"
                      WPX$err[NPX]=0.05
                      WPX$pol[NPX]=0
                      WPX$flg[NPX]=999
                      WPX$res[NPX]=0
                      WPX$yr[NPX]=NH$info$yr[i1]
                      WPX$mo[NPX]= NH$info$mo[i1]
                      WPX$dom[NPX]=NH$info$dom[i1]
                      WPX$jd[NPX]=NH$info$jd[i1]
                      WPX$hr[NPX]=NH$info$hr[i1]
                      WPX$mi[NPX]=NH$info$mi[i1]
                      WPX$sec[NPX]=asec
                      WPX$col[NPX]="purple"
                      WPX$onoff[NPX] = 1 
          

          PULSPX[[NPX]] =  list( pick=c(NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick], NH$info$mi[ipick], asec), kind="SHAPE", sta= STNS[ypick], comp=COMPS[ypick], dispcomp=COMPS[ypick], PULS=PULS)

          dev.set( MAINdev)
          zloc = list(x=NULL, y=NULL)
        }
      
      ###################   FREQUENCY ANALYSIS   ###########################      
 
      if(K[Nclick]==match("SPEC.old", BLABS, nomatch = NOLAB) & zenclick>=1)
        {
         ###  u = par("usr")


          if(zenclick>=3)
            {
              ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
              ipick = sel[ypick]
             ### print(paste(sep=' ',ypick, NH$info$name[ ipick]))
              pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))
            }
          if(zenclick==2)
            {
              ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
              ipick = sel[ypick]
            ###  print(paste(sep=' ',ypick, NH$info$name[ipick]))
              pwin = WIN
            }
          
          if(zenclick==1)
            {
              ypick =1
              ipick = sel[ypick]
              pwin = LASTwin
            }
          
          LASTwin = pwin
   
        ###  print(paste(sep=' ',ypick, NH$info$name[ ipick]))

          famp = NH$JSTR[[ipick]]
          ###  need to flip the accoustic trace?

       ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(famp) )
          
          temp =  famp[ ex > pwin[1] & ex <pwin[2]]
          
          g  =  list(y=temp-mean(temp), dt=NH$dt[ipick])
          ### g$x = NH$ex[ NH$ex > zloc$x[zenclick-2] & NH$ex <zloc$x[zenclick-1]]

          ###get(getOption("device"))()
          dev.new()

          
          ### X11()
          f1 = 0.1
          f2 = floor(0.33*(1/NH$dt[ipick]))

          
           Spec = MTMplot(g, f1, f2, PLOT=TRUE)
          dev.set( MAINdev)
  
          ###  dev.off(dev.cur())
    ###   source("/home/lees/Progs/R_stuff/tung.R");
          ###     a = PICK.MARIO(GH,  sel, WIN=twin) 
          zloc = list(x=NULL, y=NULL)
          
        }

######################################################################

      if(K[Nclick]==match("ASCII", BLABS, nomatch = NOLAB) & zenclick>=1)
        {  ########ascii

          if(zenclick>=3)
            {
              nc = 1:(zenclick-1)
              lnc = length(nc)
              
              ypick = length(sel)-floor(length(sel)*zloc$y[nc])
              ipick = sel[ypick]
### print(paste(sep=' ',ypick, NH$info$name[ ipick]))

              print(ipick)
              
              i1 = seq(from=1, to=max(nc), by=2)
              i1 = i1[i1<max(nc)]
              amp = vector()
              dees = list()
              stamps =  vector()
              speccol = vector()
              ni = 0

              
              for(ipix in i1)
                {
                  pwin = sort(c(zloc$x[ipix], zloc$x[ipix+1]))
                  print(c(ipix, pwin))

                  kpix = ipick[ipix]

                  famp = NH$JSTR[[kpix]]
	          dsec=NH$dt[kpix]

                  ex = seq(from=NH$info$t1[kpix], by=NH$info$dt[kpix], length.out=length(famp) )

                  
                  temp =  famp[ ex > pwin[1] & ex <pwin[2]]
                  ni = ni +1

                  amp = temp-mean(temp)

                  
                  ftime = Zdate(NH$info, kpix, pwin[1])
                  psta = NH$STNS[kpix]
                  pcomp =  NH$COMPS[kpix]

                  ampfn = paste(sep=".", ftime,psta,pcomp, "asc")
                  
                  cat(file=ampfn, paste(sep=" ", ftime , psta , pcomp, dsec, length(amp), "\n"))
                  ##   cat(file=ampfn, sep="\n", append=TRUE)
                  cat(file=ampfn, sep="\n", amp, append=TRUE)
                  ##   print(paste(sep=" ", NH$extras$LEVEL,  NH$extras$PICKER))	

                  
                }

              
            }
          if(zenclick==2)
            {
              ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
              ipick = sel[ypick]
###  print(paste(sep=' ',ypick, NH$info$name[ipick]))
              pwin = WIN
            }
          
          if(zenclick==1)
            {
              ypick =1
              ipick = sel[ypick]
              pwin = LASTwin
            }
          
          LASTwin = pwin
          
         zloc = list(x=NULL, y=NULL) 
        }

######################################################################
########  FUJITA-SAN: here  I added inb code for amplitude analysis

      ###################   AMPLITUDE ANALYSIS   ###########################   

  if(K[Nclick]==match("AMPL", BLABS, nomatch = NOLAB) & zenclick>=1)
        {  #####AMPL
###  u = par("usr")

          
          nowdate = format(Sys.time(), "%Y_%m_%d")
          
          ampfn = paste(sep=".", 	nowdate, "AMPL")
          
          CAPP = file.exists(ampfn)
          
          
          
          if(zenclick>=3)
            {
              nc = 1:(zenclick-1)
              lnc = length(nc)
              
              ypick = length(sel)-floor(length(sel)*zloc$y[nc])
              ipick = sel[ypick]
### print(paste(sep=' ',ypick, NH$info$name[ ipick]))
              
              print(ipick)
              
              i1 = seq(from=1, to=max(nc), by=2)
              i1 = i1[i1<max(nc)]
              amp = vector()
              dees = list()
              stamps =  vector()
              speccol = vector()
              ni = 0
              
              
              for(ipix in i1)
                {
                  pwin = sort(c(zloc$x[ipix], zloc$x[ipix+1]))
                  print(c(ipix, pwin))
                  kpix = ipick[ipix]
                  famp = NH$JSTR[[kpix]]
                  
                  
                  ex = seq(from=NH$info$t1[kpix], by=NH$info$dt[kpix], length.out=length(famp))
                  temp =  famp[ ex > pwin[1] & ex <pwin[2]]
                  
                  ni = ni +1

                  amp = temp-mean(temp)
                  
                  MSSA = sum(amp*amp)/length(amp)	
                  ##    VA = var(amp)	
                  
                  
                  
                  
                  ftime = Zdate(NH$info, kpix, pwin[1])
                  psta = NH$STNS[kpix]
                  pcomp =  NH$COMPS[kpix]
                  STAMP = paste(sep=" ", psta, pcomp, ftime, MSSA)
                  stamps[ni] = STAMP
                  
                }
              
              print(stamps)
              cat(stamps, file=ampfn,sep="\n", append=CAPP)
              
            }
          if(zenclick==2)
            {
              ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
              ipick = sel[ypick]
###  print(paste(sep=' ',ypick, NH$info$name[ipick]))
              pwin = WIN
            }
          
          if(zenclick==1)
            {
              ypick =1
              ipick = sel[ypick]
              pwin = LASTwin
            }
          
          LASTwin = pwin
          zloc = list(x=NULL, y=NULL) 
          
        }

################################################
###################   ternary filtered AMPLITUDE ANALYSIS   ###########################      

  if(K[Nclick]==match("TRNAMPL", BLABS, nomatch = NOLAB) & zenclick>=1)
    {
###  u = par("usr")


      nowdate = format(Sys.time(), "%Y_%m_%d")

      ampfn = paste(sep=".", 	nowdate, "TERNAMPL")
      
      CAPP = file.exists(ampfn)



      if(zenclick>=3)
        {
          nc = 1:(zenclick-1)
          lnc = length(nc)
              
          ypick = length(sel)-floor(length(sel)*zloc$y[nc])
          ipick = sel[ypick]
### print(paste(sep=' ',ypick, NH$info$name[ ipick]))

              print(ipick)
              
              i1 = seq(from=1, to=max(nc), by=2)
              i1 = i1[i1<max(nc)]
              amp = vector()
              dees = list()
              stamps =  vector()
              speccol = vector()
              ni = 0

              
              for(ipix in i1)
                {
                  pwin = sort(c(zloc$x[ipix], zloc$x[ipix+1]))
                  print(c(ipix, pwin))
                  kpix = ipick[ipix]
                  famp = NH$JSTR[[kpix]]
	          dsec=NH$dt[kpix]

                  ex = seq(from=NH$info$t1[kpix], by=NH$info$dt[kpix], length.out=length(famp))

                  
                  temp =  famp[ ex > pwin[1] & ex <pwin[2]]
                  ni = ni +1

                  amp = temp-mean(temp)

			MSSA0 = sum(amp*amp)/length(amp)	
	
		fy1 = butfilt(famp, fl=1, fh=3.5 , dsec , "BP", "BU" )
                 
 
                  temp =  fy1[ ex > pwin[1] & ex <pwin[2]]
                  

                  ampf1 = temp-mean(temp)

			MSSA1 = sum(ampf1*ampf1)/length(ampf1)	


	fy1 = butfilt(famp, fl=3.5, fh=7 , dsec , "BP", "BU" )
                 
 
                  temp =  fy1[ ex > pwin[1] & ex <pwin[2]]
                  

                  ampf1 = temp-mean(temp)

			MSSA2 = sum(ampf1*ampf1)/length(ampf1)	


	fy1 = butfilt(famp, fl=7, fh=12 , dsec , "BP", "BU" )
                 
 
                  temp =  fy1[ ex > pwin[1] & ex <pwin[2]]
                  

                  ampf1 = temp-mean(temp)

			MSSA3 = sum(ampf1*ampf1)/length(ampf1)	





                  ftime = Zdate(NH$info, kpix, pwin[1])
                  psta = NH$STNS[kpix]
                  pcomp =  NH$COMPS[kpix]

         

             ##   print(paste(sep=" ", NH$extras$LEVEL,  NH$extras$PICKER))	

                  STAMP = paste(sep=" ", psta, pcomp, ftime, NH$extras$LEVEL,  NH$extras$PICKER, MSSA0,  MSSA1,  MSSA2, MSSA3  )
                  stamps[ni] = STAMP
                  
                }

              print(stamps)
              cat(stamps, file=ampfn,sep="\n", append=CAPP)
              
            }
          if(zenclick==2)
            {
              ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
              ipick = sel[ypick]
            ###  print(paste(sep=' ',ypick, NH$info$name[ipick]))
              pwin = WIN
            }
          
          if(zenclick==1)
            {
              ypick =1
              ipick = sel[ypick]
              pwin = LASTwin
            }
          
          LASTwin = pwin
   
      zloc = list(x=NULL, y=NULL) 
        }

################################################
################################################
################################################
      if(K[Nclick]==match("SPEC", BLABS, nomatch = NOLAB) & zenclick>=1)
        {
         ###  u = par("usr")


          if(zenclick>=3)
            {
              nc = 1:(zenclick-1)
              lnc = length(nc)
              
              ypick = length(sel)-floor(length(sel)*zloc$y[nc])
              ipick = sel[ypick]
             ### print(paste(sep=' ',ypick, NH$info$name[ ipick]))

              print(ipick)
              
              i1 = seq(from=1, to=max(nc), by=2)
              i1 = i1[i1<max(nc)]
              amp = list()
              dees = list()
              stamps =  list()
              speccol = vector()
              ni = 0

              for(ipix in i1)
                {
                  pwin = sort(c(zloc$x[ipix], zloc$x[ipix+1]))
                  print(c(ipix, pwin))

                  kpix = ipick[ipix]

                  famp = NH$JSTR[[kpix]]


                  ex = seq(from=NH$info$t1[kpix], by=NH$info$dt[kpix], length.out=length(famp) )
                  temp =  famp[ ex > pwin[1] & ex <pwin[2]]


                  if(any(is.na(temp)))
                    {
                      print(paste("getting NA in trace",kpix, NH$STNS[kpix],NH$COMPS[kpix],pwin[1],  pwin[2]  ))
                      next
                    }
                  
                  ni = ni +1

                  amp[[ni]] = temp-mean(temp)
                  dees[ni] = NH$dt[kpix]

                  speccol[ni] = pcols[kpix]

                  ftime = Zdate(NH$info, kpix, pwin[1])
                  psta = NH$STNS[kpix]
                  pcomp =  NH$COMPS[kpix]
                  STAMP = paste(sep=" ", psta, pcomp, ftime)
                  stamps[ni] = STAMP
                  
                }

              print(stamps)
              a = list(y=amp, dt=dees, stamps=stamps)

              if(length(a$y)>0)
                 {
                   dev.new(width=10, height=10)
                   
                   f1 = 0.1
                   f2 = floor(0.33*(1/NH$dt[ipick]))
                   
                 ###  oop=par(no.readonly = TRUE)
                 ###  par(mfrow=c(length(a$y), 1) )
                 ###  for(io in 1:length(a$y)) plot(a$y[[io]], type='l')
                 ###  par(oop)
                 ###  readline("type in something")
                   
                   MTM.drive(a, f1, f2[1], COL=speccol, PLOT=TRUE)
                 }
                 dev.set( MAINdev)
               }
              else
            {
          
         
              cat("SPEC WARNING: no window or trace has been selected:", sep="\n")
              ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
              ipick = sel[ypick]
###  print(paste(sep=' ',ypick, NH$info$name[ipick]))
              pwin = WIN
              
            }
          
          LASTwin = pwin
   
               zloc = list(x=NULL, y=NULL) 
        }
                          
      ###################   SPECTROGRAM  ANALYSIS   ###########################      
 
      if(K[Nclick]==match("SGRAM", BLABS, nomatch = NOLAB) & zenclick>=1)
        {
###  u = par("usr")

          if(zenclick>=2)
            {
              if(zenclick==2)
                {
                  ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
                  ipick = sel[ypick]
###  print(paste(sep=' ',ypick, NH$info$name[ipick]))
                  pwin = WIN
                }
              else
                {
                  
                  ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
                  ipick = sel[ypick]
### print(paste(sep=' ',ypick, NH$info$name[ ipick]))
                  pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))
                }
           
          LASTwin = pwin
   
          ### print(paste(sep=" ", "DOING SGRAM  Nclick, ipick, pwin", Nclick, ipick, pwin))
          
          famp = NH$JSTR[[ipick]]
         
          ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(famp))
         
          temp =  famp[ ex > pwin[1] & ex <pwin[2]]

          Xamp =   temp-mean(temp)

          ftime = Zdate(NH$info, sel[ypick], pwin[1])
	print(paste(sep=" ",min(ex), max(ex)))
	
	print(paste(sep=" ",pwin[1], pwin[2]))
	
          print(paste(sep=" ", ipick, length(famp),length(temp),length(Xamp), NH$dt[ipick],ftime)) 

          SPECT.drive(Xamp, DT=NH$dt[ipick], STAMP=ftime)

        ###   plotevol(DEV, log=1, fl=0, fh=15, col=rainbow(50))
           }
           else
             {
              pwin = LASTwin
              ypick = 1
              ipick = sel[1]
              cat("SGRAM WARNING: no window or trace has been selected:" , sep="\n")
            }

          dev.set(MAINdev)
            zloc = list(x=NULL, y=NULL) 
        }

      ###################   wavelet  ANALYSIS   ######################
 
      if(K[Nclick]==match("WLET", BLABS, nomatch = NOLAB) & zenclick>=1)
        {
        print(zenclick-1)
          if(zenclick>=2)
            {

          if(zenclick==2)
            {
              ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
              ipick = sel[ypick]
            ###  print(paste(sep=' ',ypick, NH$info$name[ipick]))
              pwin = WIN
            }
          else
            {
              
              ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
              ipick = sel[ypick]
          ###    print(paste(sep=' ',ypick, NH$info$name[ ipick]))
              pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))
            }

            print(NH$info$name[ipick])
          
          LASTwin = pwin
     
           
          famp = NH$JSTR[[ipick]]
         
          ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(famp))
          temp =  famp[ ex > pwin[1] & ex <pwin[2]]
          Xamp =   temp-mean(temp)


           ftime = Zdate(NH$info, sel[ypick], pwin[1])

         ###  smallex = NH$ex[ NH$ex > pwin[1] & NH$ex <pwin[2]]
          
           if(require("Rwave")==TRUE)
             {
            
              wlet.drive(Xamp, NH$dt[ipick], STAMP=ftime)
             }
          else
            {
              
              cat("WLET WARNING: NEED package RWAVE to do wavelet analysis (not available on MAC", sep="\n")
              
             }
          
          
          dev.set( MAINdev)
#######  source("/home/lees/Progs/R_stuff/PICK.R"); save.image()
        }
          else
            {
              
              pwin = LASTwin
              ypick = 1
              ipick = sel[1]
              cat("WLET WARNING: no window or trace has been selected:", sep="\n")
              
            }


        
          zloc = list(x=NULL, y=NULL) 
        }
            ###################   filter stuff  ANALYSIS   ###########################      
 
      if(K[Nclick]==match("FILT", BLABS, nomatch = NOLAB))
        {####FILT

           Fdef = choosfilt(filters)

           if(!is.null(Fdef))
             {

           if(Fdef$type=="None")
             {
               if(exists("OLDH")) 
               NH = OLDH
             }
           else
             {
                OLDH=NH
               KF = FILT.SEISN(NH, sel = sel, FILT=Fdef)
               NH = KF
             }
           ###  X11()
         }
           dev.set( MAINdev)

            YN = YNreplot()
         
           buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
          ###  dev.set( MAINdev)  
          zloc = list(x=NULL, y=NULL) 
         }


      if(K[Nclick]==match("UNFILT", BLABS, nomatch = NOLAB))
        {

         
          if(exists("OLDH"))NH = OLDH
          dev.set( MAINdev)

            YN = YNreplot()
      
           buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
          ###  dev.set( MAINdev)  
        zloc = list(x=NULL, y=NULL) 
         }




      
 ###########################      
      if(K[Nclick]==match("BRUNE", BLABS, nomatch = NOLAB) & zenclick>=3)
        {
          
###  u = par("usr")
          ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
          ipick = sel[ypick]
         ###  print(paste(sep=' ',"look at Brune", ypick, NH$info$name[ ipick]))
          
          famp = NH$JSTR[[ipick]]
###  need to flip the accoustic trace?
          
          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))


           ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(famp) )
          temp =  famp[ ex > pwin[1] & ex <pwin[2]]
          Xamp =   temp-mean(temp)

          dev.set(MAINdev+1)
         ##  dev.set(dev.next())
                 f1 = 0.01
              f2 = floor(0.25*(1/NH$dt[ipick]))
         ###    }
          
          BF = brune.doom( Xamp, NH$dt[ipick] ,f1=f1, f2=f2 ,  PLOTB = TRUE)
          BRUNKOUNT = BRUNKOUNT+1
          BRUNINFO[[BRUNKOUNT]] = list(stn=NH$STNS[ipick], comp=NH$COMPS[ipick] ,  pwin=pwin, BF=BF)
          print(BF) 
          NH$KNOTES[ipick] = paste(sep=" ", "BR", NH$KNOTES[ipick])
          dev.set( MAINdev)
#######  source("PICK.R"); save.image()
          zloc = list(x=NULL, y=NULL) 
        }
 ###########################      
###########################      
      if(K[Nclick]==match("DETAIL", BLABS, nomatch = NOLAB) & zenclick>=3)
        {
          screens(2)
          
###  u = par("usr")
          print("Going to  DETAIL")
        
          ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
          ipick = sel[ypick]
          
          
          famp = NH$JSTR[[ipick]]

          
          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))

          ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(famp))
          
          temp =  famp[ ex > pwin[1] & ex <pwin[2]]

          ex = ex[ ex > pwin[1] & ex <pwin[2]]
  
          ##  X11()
          
          dev.set(MAINdev+1)
          dsec  = NH$dt[ipick]
         ##  fh=0.12*(1/dsec)
         ##  fl = 0.5
         ##  fy = butfilt(temp, fl=fl, fh=fh , dsec , "BP", "BU" )

         ##   if(exists("DETAIL.FILT"))
          ##    {
          ##      fh=DETAIL.FILT[2]
          ##      fl =DETAIL.FILT[1]
          ##      fy = butfilt(temp, fl=fl, fh=fh , dsec , "BP", "BU" )
         ##     }
         ##     else
         ##     {

          ########   this is a default - need to have some way to store a
         ######## database of default parameters...
               fh=  0.12*(1/dsec)
               fl = 1/100
               fy = temp
         ##     }
  
          
          
          Xamp =   fy-mean(fy)
          
          KSAVE = detail.pick(Xamp, ex,  dsec, TIT=NH$KNOTES[ipick])

          DETLKOUNT = DETLKOUNT+1
          DETLINFO[[DETLKOUNT]] = list(stn=NH$STNS[ipick], comp=NH$COMPS[ipick] ,  pwin=pwin, points=KSAVE, filt=c(fl,fh))

          ##  dev.off()
          NH$KNOTES[ipick] = paste(sep=" ", "DT", NH$KNOTES[ipick])
          dev.set( MAINdev)

           zloc = list(x=NULL, y=NULL) 
        }
 ###########################      



 
      if(K[Nclick]==match("PTS", BLABS, nomatch = NOLAB))
        {
         pts=!pts

           YN = YNreplot()
         
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
          zloc = list(x=NULL, y=NULL) 
        }


      #######  source("/home/lees/Progs/R_stuff/PICK.R"); save.image()
 ###########################      

      if(K[Nclick]==match("MMARKS", BLABS, nomatch = NOLAB))
        {
        pwin = WIN
        ex  =  NH$ex[NH$ex > pwin[1] & NH$ex <pwin[2]]

        #########  ??????????????????????????  ##################
   ####  asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]
         zloc = list(x=NULL, y=NULL) 
        }


      #######  source("/home/lees/Progs/R_stuff/PICK.R"); save.image()

#######  source("PICK.R"); save.image()
      if(K[Nclick]==match("PMOT", BLABS, nomatch = NOLAB))
        {
          ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
          ipick = sel[ypick]
   
          psta = NH$STNS[ipick]

          ista = psta == NH$STNS
          
          
          Iv =  which(ista &  ("V"==NH$COMPS| "v"==NH$COMPS | "4"==NH$COMPS |"Vertical"==NH$COMPS |"U"==NH$COMPS) )
          In =  which(ista & ("N"==NH$COMPS | "n"==NH$COMPS | "5"==NH$COMPS |"North"==NH$COMPS) )
          Ie =  which(ista & ("E"==NH$COMPS | "e"==NH$COMPS | "6"==NH$COMPS |"East"==NH$COMPS) )

          print(paste(sep=' ', "pmot get comps: ", Iv, In, Ie))
         
          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))


        
          ex1 = seq(from=NH$info$t1[Iv], by=NH$info$dt[Iv], length.out=length( NH$JSTR[[Iv]]) )
          ex2 = seq(from=NH$info$t1[In], by=NH$info$dt[In], length.out=length(NH$JSTR[[In]]))
          ex3 = seq(from=NH$info$t1[Ie], by=NH$info$dt[Ie], length.out=length(NH$JSTR[[Ie]]))
         
       
          temp =  cbind(NH$JSTR[[Iv]][ex1 > pwin[1] & ex1 <pwin[2]],
            NH$JSTR[[In]][ex2 > pwin[1] & ex2 <pwin[2]],
            NH$JSTR[[Ie]][ex3 > pwin[1] & ex3 <pwin[2]])

         #####   ex  =  ex[ex > pwin[1] & ex <pwin[2]]
          

          pmolabs=NH$COMPS[c(Iv, In, Ie)]

         ### get(getOption("device"))(width=15, height=10)
          dev.new(width=15, height=10)
        ###  X11(width=15, height=10)
          
          ###########    PMOtrace(temp, WIN=NULL, labs=pmolabs, PS=FALSE, ID="")
          ftime = Zdate(NH$info, sel[ypick], pwin[1])

          STAMP = paste(sep=" ", psta, ftime)
          #####  simple.hodo(temp,labs=pmolabs, COL=rainbow(100))
          PMOT.drive(temp, NH$dt[1], pmolabs, STAMP=STAMP)

          dev.set( MAINdev)
          
           zloc = list(x=NULL, y=NULL) 
        }



      if(K[Nclick]==match("STERNET", BLABS, nomatch = NOLAB))
        {
          ####  plot an equal area steronet with the points of the 3-comp seismogram
          ####           on the stereonet
          ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
          ipick = sel[ypick]
   
          psta = NH$STNS[ipick]

          ista = psta == NH$STNS
          
           Iv =  which(ista &  ("V"==NH$COMPS| "v"==NH$COMPS | "4"==NH$COMPS |"Vertical"==NH$COMPS |"U"==NH$COMPS) )
          In =  which(ista & ("N"==NH$COMPS | "n"==NH$COMPS | "5"==NH$COMPS |"North"==NH$COMPS) )
          Ie =  which(ista & ("E"==NH$COMPS | "e"==NH$COMPS | "6"==NH$COMPS |"East"==NH$COMPS) )

          print(paste(sep=' ', "pmot get comps: ", Iv, In, Ie))
            
        
          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))

    ex1 = seq(from=NH$info$t1[Iv], by=NH$info$dt[Iv], length.out=length( NH$JSTR[[Iv]]) )
          ex2 = seq(from=NH$info$t1[In], by=NH$info$dt[In], length.out=length(NH$JSTR[[In]]))
          ex3 = seq(from=NH$info$t1[Ie], by=NH$info$dt[Ie], length.out=length(NH$JSTR[[Ie]]))
         

         
        
         
          temp =  cbind(NH$JSTR[[Iv]][ex1 > pwin[1] & ex1 <pwin[2]],
            NH$JSTR[[In]][ex2 > pwin[1] & ex2 <pwin[2]],
            NH$JSTR[[Ie]][ex3 > pwin[1] & ex3 <pwin[2]])

         

          pmolabs=NH$COMPS[c(Iv, In, Ie)]

          ###get(getOption("device"))(width=6, height=6)
          dev.new(width=6, height=6)
         ###  X11(width=6, height=6)
          
          ###########    PMOtrace(temp, WIN=NULL, labs=pmolabs, PS=FALSE, ID="")
          ftime = Zdate(NH$info, sel[ypick], pwin[1])
          STAMP = paste(sep=" ", psta, ftime)
          
          #####  
         SNET.drive(temp, STAMP=STAMP)
          dev.set( MAINdev)
          
           zloc = list(x=NULL, y=NULL) 
        }

#######  source("PICK.R"); save.image()
      if(K[Nclick]==match("GTAZI", BLABS, nomatch = NOLAB))
        {
          ypick = length(sel)-floor(length(sel)*zloc$y[zenclick-1])
          ipick = sel[ypick]

          
          psta = NH$STNS[ipick]

          ista = psta == NH$STNS
          
        
          Iv =  which(ista &  ("V"==NH$COMPS| "v"==NH$COMPS | "4"==NH$COMPS |"Vertical"==NH$COMPS |"U"==NH$COMPS) )
          In =  which(ista & ("N"==NH$COMPS | "n"==NH$COMPS | "5"==NH$COMPS |"North"==NH$COMPS) )
          Ie =  which(ista & ("E"==NH$COMPS | "e"==NH$COMPS | "6"==NH$COMPS |"East"==NH$COMPS) )

          print(paste(sep=' ', "pmot get comps: ", Iv, In, Ie))
          
          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))


ex1 = seq(from=NH$info$t1[Iv], by=NH$info$dt[Iv], length.out=length( NH$JSTR[[Iv]]) )
          ex2 = seq(from=NH$info$t1[In], by=NH$info$dt[In], length.out=length(NH$JSTR[[In]]))
          ex3 = seq(from=NH$info$t1[Ie], by=NH$info$dt[Ie], length.out=length(NH$JSTR[[Ie]]))
        
          
     
        
         
          temp =  cbind(NH$JSTR[[Iv]][ex1 > pwin[1] & ex1 <pwin[2]],
            NH$JSTR[[In]][ex2 > pwin[1] & ex2 <pwin[2]],
            NH$JSTR[[Ie]][ex3 > pwin[1] & ex3 <pwin[2]])

         
          ftime = Zdate(NH$info, sel[ypick], pwin[1])
          GAZISTAMP = paste(sep=" ", psta, ftime)
          
          pmolabs=NH$COMPS[c(Iv, In, Ie)]
         #### get(getOption("device"))()
          dev.new()
         ####  X11()
          
          
          G  = GAZI(temp, dt =NH$dt[Iv]  , comp = pmolabs, sta = psta , az = 0, len =75, shift = 10, prev = 1)

          dev.set( MAINdev)
          
           zloc = list(x=NULL, y=NULL) 
        }
      

  if(K[Nclick]==match("ENVLP", BLABS, nomatch = NOLAB))
        {
          #### 
          ####         
          ypick = length(sel)-floor(length(sel)*zloc$y[1:(zenclick-1)])
          ipick = sel[ypick]
   
          psta = NH$STNS[ipick]

          print(ypick)
          print(ipick)
          pwin = WIN
          print(pwin)
          
           ex1 = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(NH$JSTR[[ipick]]) )
          ex  =  NH$ex[ex1 > pwin[1] & ex1 <pwin[2]]

          
          Y = matrix(nrow = length(ex), ncol = length(ipick) )
          stamps = vector()
          ftime = Zdate(NH$info, 1, pwin[1])
          for(i in 1:length(ipick))
            {
              Y[,i] = NH$JSTR[[ipick[i]]][ex1>=pwin[1]&ex1<=pwin[2]]
              psta = NH$STNS[ipick[i]]
              pcomp =  NH$COMPS[ipick[i]]
              STAMP = paste(sep=" ", psta, pcomp, ftime)
              stamps[i] = STAMP
         
            }
         
         print(stamps)


         ### get(getOption("device"))(width=6, height=6)
          dev.new(width=6, height=6)
         ###  X11(width=6, height=6)
          
          ###########   
          comp.env(ex, Y, stamps=stamps)

          #####  
        
          dev.set( MAINdev)
           zloc = list(x=NULL, y=NULL) 
          
        }

###################   Window Information   ###########################      
 
      if(K[Nclick]==match("WINFO", BLABS, nomatch = NOLAB))
        {
          
          ###   print out information on all traces in the window
          if(is.null(WIN))
            {
              u = par('usr')
              WIN = c(u[1], u[2])
            }
          pwin = WIN
          
          
          print(pwin)

          rd = list(stn='', comp='', yr=0, jd=0, hr=0, mi=0, sec=0,  dt=0, N=0, min=0, max=0 ,var=0, mean=0, oct1=0, qrt1=0,
            med=0, qrt2=0, oct2=0, blank=FALSE)
          
          for(i  in  1:length(sel))
            {
              
              ipick = sel[i]
              
              asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+pwin[1]
              spaz = recdate( NH$info$jd[ipick], NH$info$hr[ipick], NH$info$mi[ipick], asec,  NH$info$yr[ipick] )

               rd$jd[i]=spaz$jd;  rd$hr[i]= spaz$hr ;  rd$mi[i]=spaz$mi ;  rd$sec[i]=spaz$sec;

              rd$yr[i] =   as.integer(NH$info$yr[ipick])
              
              rd$stn[i] =  NH$STNS[ipick]
              rd$comp[i] = NH$COMPS[ipick]
              rd$dt[i] = NH$info$dt[ipick]

              
              ex = seq(from=NH$info$t1[ipick], by=NH$info$dt[ipick], length.out=length(NH$JSTR[[ipick]]))
              
              a = NH$JSTR[[ipick]][ex>=pwin[1]&ex<=pwin[2]]
              
              rd$N[i] = length(a)
              rd$min[i] = min(a,  na.rm=TRUE)
              rd$max[i] = max(a,  na.rm=TRUE)
              rd$mean[i] = mean(a,  na.rm=TRUE)

              

              bbox = boxplot(a, plot=FALSE)
              
              rd$med[i] = bbox$stats[3]
              rd$oct1[i] = bbox$stats[1]
              rd$oct2[i] = bbox$stats[5]
              rd$qrt1[i] = bbox$stats[2]
              rd$qrt2[i] = bbox$stats[4]
              
              
              rd$var[i] = var(a,   na.rm=TRUE)
              
              rd$blank[i] = any(is.na(a))
              
            }

          options(width=180)

            print(data.frame(rd))
           zloc = list(x=NULL, y=NULL) 
        }

   ###################
    if(K[Nclick]==match("RESHIFT", BLABS, nomatch = NOLAB))
        {
          
          ASHIFT = BLAHSHIFT
           YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)

          sloc = zloc
          zloc = list(x=NULL, y=NULL)
        
          
          
        }
  
            ################### TINE SHIFT = TSHIFT or aline or align traces  ###########################      
 
     if(K[Nclick]==match("TSHIFT", BLABS, nomatch = NOLAB))
        {

          if(zenclick>=2)
            {
              
              kix = legitpix(sel, zloc, zenclick)

              
              ypick =  kix$ypick
              ppick = kix$ppick
      
              dpick = c(0, diff(ppick))
              ipick = sel[ypick]

              print(paste(NH$STNS[ipick], NH$COMPS[ipick]))

              
              tshft = rep(0,times=length(NH$STNS))

              
              tshft[ipick] = ppick-ppick[1]
              
              ## print(data.frame(list(sta=NH$STNS, comp=NH$COMPS, tshft=tshft)))

              print(data.frame(list(sta=NH$STNS[ipick] , comp=NH$COMPS[ipick] , tshft=tshft[ipick] )))
              
              ASHIFT = tshft
               BLAHSHIFT = tshft
            }
          else
            {

              ASHIFT = SHIFT.ORIG

            }

          YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)

          sloc = zloc
          zloc = list(x=NULL, y=NULL)
        
          

        }

   
            ###################   time pick analysis   ###########################      
 
      if(K[Nclick]==match("Pinfo", BLABS, nomatch = NOLAB))
        {

          if(zenclick>=2)
            {
              
              kix = legitpix(sel, zloc, zenclick)

              
              ypick =  kix$ypick
              ppick = kix$ppick
      
              dpick = c(0, diff(ppick))
              ipick = sel[ypick]


              
              
              m = match(STNS[ipick],UNIsta)
              jj = floor((zloc$y[zenclick-1])/du)
              asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-1]

              print(paste(sep=" ", "PICK=", NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick], NH$info$mi[ipick], asec, "sta=", NH$STNS[ipick], "comp=", NH$COMPS[ipick] ))
              print(ppick)


          
              ##  pstas = paste(NH$STNS[ipick], NH$COMPS[ipick], sep=".")

              
              rd = getrdpix(zloc, zenclick, sel, NH)
             

              RDtmes = rd$yr+rd$jd/366+rd$hr/(366*24)+rd$mi/(366*24*60)+rd$sec/(366*24*3600)
              
               wearliest = which.min(RDtmes)
              PAS = paste(sep="_", "Jtim(", rd$jd[wearliest], ", hr=" , rd$hr[wearliest] ,
                ", mi=", rd$mi[wearliest], ",sec=", rd$sec[wearliest], ")")

              
              DEEtimes = YRsecdif(
                rd$jd[wearliest],rd$hr[wearliest],rd$mi[wearliest], rd$sec[wearliest],
                rd$jd,  rd$hr, rd$mi, rd$sec, rd$yr[wearliest],  rd$yr) 


              apickorg = paste(sep=",", rd$yr[wearliest], rd$jd[wearliest],rd$hr[wearliest],rd$mi[wearliest], rd$sec[wearliest])
              
              ##  pstas =  NH$STNS[ipick]

              apstas = paste(sep="", '"', paste(rd$stn, collapse='","'), '"')


              ##    pcomps =NH$COMPS[ipick]

              apcomps = paste(sep="", '"', paste(rd$comp, collapse='","'), '"')

              cat("", sep="\n")
              cat("", sep="\n")
              cat("##################", sep="\n")
              cat( paste(sep=" ", "orgtim=c(", apickorg , ")") , sep="\n")
              
              cat("", sep="\n")
             cat( paste(sep=" ", "stns=c(", apstas, ")") , sep="\n")
             cat( paste(sep=" ", "comps=c(", apcomps, ")") , sep="\n")

              
              
             cat( paste(sep=" ", "tims=c(", paste(DEEtimes, collapse=","), ")") , sep="\n")

              cat("", sep="\n")
              cat("##################", sep="\n")
              cat("", sep="\n")
              cat("Time Differences between picks:", sep="\n")
              
              cat(paste(dpick), sep="\n")

              cat("", sep="\n")
####  print(zloc$y[1:(zenclick-1)])  
####  print(ypick)     
####  print(ipick)
      
              write.table(file="", data.frame(rd), row.names =FALSE)
              cat("GMT TIME: ", sep="\n")
              showdatetime(rd)

              cat(" ", sep="\n")
              
              PAS = paste(sep=" ", "Jtim(", rd$jd, ", hr=" , rd$hr , ", mi=", rd$mi, ",sec=", rd$sec, ")")
              cat("", sep="\n")
              cat(PAS, sep="\n")


              if(!is.null(GH$TZ))
                {
                  rdlocal = recdate(jd=rd$jd, hr=rd$hr+GH$TZ, mi=rd$mi, sec=rd$sec , yr=rd$yr)
                  cat(" ", sep="\n")
                  
                  cat(paste(sep=" ", "LOCAL TIMES, SHIFT=", GH$TZ) , sep="\n")
                  showdatetime(rdlocal, AMPM=TRUE)
                  
                }




              
            }
          else
            {
              cat("Pinfo WARNING: no pick or trace has been selected:", sep="\n")
              
            }
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }

#############################################################################


      if(K[Nclick]==match("Plines", BLABS, nomatch = NOLAB))
        {  #########Plines

          if(zenclick>=2)
            {
              
              kix = legitpix(sel, zloc, zenclick)

              
              ypick =  kix$ypick
              ppick = kix$ppick

              segments(ppick, rep(0, length(ppick)) , ppick, rep(1, length(ppick))  , col="red")
              
            }
          else
            {
              cat("Pinfo WARNING: no pick or trace has been selected:", sep="\n")
              
            }
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }

   
       ################### 
      if(K[Nclick]==match("XCOR", BLABS, nomatch = NOLAB))
        {

      kix = legitpix(sel, zloc, zenclick)
      ypick =  kix$ypick
      ppick = kix$ppick
      
          
          
          dpick = c(0, diff(ppick))
           
              ipick = sel[ypick]
          print(ypick)
          print(ipick)
          ##
          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))
          if(length(ipick)>=2)
            {

              famp = NH$JSTR[[ipick[1]]]

                 ex = seq(from=NH$info$t1[ipick[1]], by=NH$info$dt[ipick[1]], length.out=length(famp))
              
              temp =  famp[ ex > pwin[1] & ex <pwin[2]]
              Xamp1 =  temp
              
          
              famp = NH$JSTR[[ipick[2]]]
                 ex = seq(from=NH$info$t1[ipick[2]], by=NH$info$dt[ipick[2]], length.out=length(famp))
              
              temp =  famp[ ex > pwin[1] & ex <pwin[2]]
              Xamp2 =  temp
              
             
              dev.new()

              ########## pshift = getphaselag2(Xamp1, Xamp2, NH$info$dt[ipick[1]] , PLOT=TRUE)


              xc = xcor2(Xamp1, Xamp2, NH$info$dt[ipick[1]], LAG =length(Xamp1), PLOT = TRUE)
              pshift = xc$lag[which.max(xc$acf)]
              
              print(paste(sep=' ' , "Shift = ", pshift))
               dev.set( MAINdev)
              
            } 
          zloc = list(x=NULL, y=NULL) 
        }

        ################### 
      if(K[Nclick]==match("PHLAG", BLABS, nomatch = NOLAB))
        {
          
  kix = legitpix(sel, zloc, zenclick)
      ypick =  kix$ypick
      ppick = kix$ppick
      

          
          dpick = c(0, diff(ppick))
           
              ipick = sel[ypick]
          print(ypick)
          print(ipick)
          ##
          pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))
          if(length(ipick)>=2)
            {

              famp = NH$JSTR[[ipick[1]]]
               ex = seq(from=NH$info$t1[ipick[1]], by=NH$info$dt[ipick[1]], length.out=length(famp))
              
              temp =  famp[ ex > pwin[1] & ex <pwin[2]]
              Xamp1 =  temp
              
          
              famp = NH$JSTR[[ipick[2]]]
                 ex = seq(from=NH$info$t1[ipick[2]], by=NH$info$dt[ipick[2]], length.out=length(famp))
           
              temp =  famp[ ex > pwin[1] & ex <pwin[2]]
              Xamp2 =  temp
              
              ###
              dev.new()
              pshift = getphaselag2(Xamp1, Xamp2,  DT=NH$info$dt[ipick[1]],  frange=c(5, 15),  PLOT=TRUE)


             
              print(paste(sep=' ' , "Shift = ", pshift))
               dev.set( MAINdev)
              
            }
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }


        ################### 
      if(K[Nclick]==match("3COMP", BLABS, nomatch = NOLAB))
        {
          if(zenclick>=2)
            {

              kix = legitpix(sel, zloc, zenclick)
              ypick =  kix$ypick
              ppick = kix$ppick
      
              ipick = sel[ypick]
              cat(paste(sep=" ", ypick, ipick), sep="\n")
              print(ipick)
              ##
              print(paste(sep=" ", "PICK=", NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick], NH$info$mi[ipick], "sta=", NH$STNS[ipick], "comp=", NH$COMPS[ipick] ))

              ma = which(!is.na(match( NH$STNS, NH$STNS[ipick])))
              
              print(cbind(NH$STNS[ma], NH$COMPS[ma]))

              
              if(length(ma==3))
                {
##### 
#####
                  dev.new(width = 12, height = 7)
                  if(length(zloc$x)>=3)
                    {
                      pwin = range(zloc$x[1:(length(zloc$x)-1)])
                    }
                  else
                    {
                      pwin = NULL
                    }
                  
                  PICK.GEN(NH, APIX=WPX, sel=ma, WIN=pwin, STDLAB=STDLAB ,SHOWONLY = FALSE, TIT=TIT)
                  dev.set( MAINdev)
                }
             
            }
          else
            {
              cat("3COMP WARNING: no trace has been selected:", sep="\n")
            }

          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }


       if(K[Nclick]==match("Predict1D", BLABS, nomatch = NOLAB))
        {  ####### Predict1D

          if(is.null(velfile) | is.null(stafile) | is.null(LOC))
            {
              
              print("No velocity/station  file available, restart with this information")
            }
          else
            {
              STAlocs = setstas(stafile)
              VEL = Get1Dvel(velfile, PLOT=FALSE)
              m = 0
            #####   RPX = list()
              
              for(ista in 1:length(NH$STNS))
                {
                    
                    m1 = match(NH$STNS[ista], STAlocs$name)
                    
                    if(length(m1)<1) next
                    if(is.na(m1)) next
                    print(paste(sep=' ', ista, m1, NH$STNS[ista], LOC$lon,  LOC$lat, STAlocs$name[m1], STAlocs$lon[m1], STAlocs$lat[m1]))
                    
                    Dis1 = GreatDist(LOC$lon,  LOC$lat , STAlocs$lon[m1], STAlocs$lat[m1] )
                    dis = Dis1$dkm
                    hpz  =  LOC$z
                    stz  = STAlocs$z[m1]
                    mytt = travel.time1D( dis, hpz, stz,  length(VEL$zp), VEL$zp, VEL$vp );
                    
                      NPX = NPX+1

                    Nn = names(WPX)
                    WPX =rbind(WPX, rep(NA, length(Nn)))
  
                  

                    WPX$tag[NPX]=paste(sep=".",NH$STNS[i1],  NH$COMPS[i1])
                    WPX$name[NPX]=NH$STNS[i1]
                    WPX$comp[NPX]=NH$COMPS[i1]
                    WPX$c3[NPX]=NH$OCOMPS[i1]
                    WPX$phase[NPX]="P"
                      WPX$err[NPX]=0.05
                      WPX$pol[NPX]=0
                      WPX$flg[NPX]=999
                      WPX$res[NPX]=0
                      WPX$yr[NPX]=LOC$yr
                      WPX$mo[NPX]= LOC$mo
                      WPX$dom[NPX]=LOC$dom
                      WPX$jd[NPX]=LOC$jd
                      WPX$hr[NPX]=LOC$hr
                      WPX$mi[NPX]=LOC$mi
                      WPX$sec[NPX]=LOC$sec+mytt$tt
                      WPX$col[NPX]="purple"
                      WPX$onoff[NPX] = 1 
                    
                    mytt = travel.time1D( dis, hpz, stz,  length(VEL$zs), VEL$zs, VEL$vs ); 
                    
                    NPX = NPX+1

                    Nn = names(WPX)
                    WPX =rbind(WPX, rep(NA, length(Nn)))
  
                    
                      WPX$tag[NPX]=paste(sep=".",NH$STNS[i1],  NH$COMPS[i1])
                    WPX$name[NPX]=NH$STNS[i1]
                    WPX$comp[NPX]=NH$COMPS[i1]
                    WPX$c3[NPX]=NH$OCOMPS[i1]
                    WPX$phase[NPX]="S"
                      WPX$err[NPX]=0.05
                      WPX$pol[NPX]=0
                      WPX$flg[NPX]=999
                      WPX$res[NPX]=0
                      WPX$yr[NPX]=LOC$yr
                      WPX$mo[NPX]= LOC$mo
                      WPX$dom[NPX]=LOC$dom
                      WPX$jd[NPX]=LOC$jd
                      WPX$hr[NPX]=LOC$hr
                      WPX$mi[NPX]=LOC$mi
                      WPX$sec[NPX]=LOC$sec+mytt$tt
                      WPX$col[NPX]="purple"
                      WPX$onoff[NPX] = 1 
               
                    
                  }
              

            }


            K[Nclick] = 0  
          zloc = list(x=NULL, y=NULL) 

        }
  ####################################### ################### ##################  
  ####################################### ################### ##################  
 ########  source("PICK.R") ; save.image()
 
      if(K[Nclick]==match("SavePF", BLABS, nomatch = NOLAB))
        {
          
        ###  print(NH$pickfile)
         
          ###opix = vector()
          #########  check current pickfile and
          ###########    clean up non-conforming records
          Apf =  cleanpickfile(NH$pickfile)
          #############   make modifications to the pickfile here

          sex1 = WPX$sec[WPX$onoff>=0]
          sexrec = recdate(WPX$jd[WPX$onoff>=0], WPX$hr[WPX$onoff>=0],  WPX$mi[WPX$onoff>=0] , sex1, WPX$yr[WPX$onoff>=0])
          sex2 = sexrec$sec
         #### print(data.frame(list(sta=WPX$tag[WPX$onoff>=0], min=WPX$mi[WPX$onoff>=0], sec1=sex1, sec2=sex2)))

          Apf$STAS$tag=WPX$tag[WPX$onoff>=0]
          Apf$STAS$name=WPX$name[WPX$onoff>=0]
          Apf$STAS$comp=WPX$comp[WPX$onoff>=0]
          Apf$STAS$c3=WPX$c3[WPX$onoff>=0]
          Apf$STAS$phase=WPX$phase[WPX$onoff>=0]
          
          Apf$STAS$sec=sex2

          
          Apf$STAS$err=WPX$err[WPX$onoff>=0]
          Apf$STAS$pol=WPX$pol[WPX$onoff>=0]
          Apf$STAS$flg=WPX$flg[WPX$onoff>=0]
          Apf$STAS$res=WPX$res[WPX$onoff>=0]

         #### Apf$STAS$lat=WPX$lat[WPX$onoff>=0]
          ####Apf$STAS$lon=WPX$lon[WPX$onoff>=0]
         #### Apf$STAS$z=WPX$z[WPX$onoff>=0]



          
          sats =fixUWstasLL(Apf$STAS, NH$stafile)
          Apf$STAS = sats
          
 #####"lat"   "lon"   "z"

          ###  need the lat-lon zee for 

          

          
#############   
          Lc  =  length(Apf$comments)
          Apf$comments[Lc+1] = paste("Repicked", date())
          USERNAME=Sys.getenv("USERNAME")
          HOST=Sys.getenv("HOST")
          Apf$comments[Lc+2] = paste(sep=" ", "BY:",USERNAME, HOST )

          
##########   next add in pick tokens that are new

          #########  make a backup o fh old pickfile:

          isitthere = file.exists(Apf$filename)
          if(isitthere)
            {
              oldpf = paste(sep="", Apf$filename, "OLD")
              system(paste(sep=" ", "/bin/cp " , Apf$filename, oldpf), ignore.stderr = TRUE)
            }
         #####     output=paste(sep=".", Apf$filename, "temp")

          output=Apf$filename
          print(output)
          print(WPX)
          
        #####   writeUWpickfile(Apf, output=output)
          GH = NH
          GH$pickfile = Apf
          rootnameID = GH$pickfile$UWFILEID
          if(is.null(GH$RFilename)) { GH$RFilename = paste(sep="", rootnameID, "R") }

          print(paste("getting ready to overwrite the GH file", GH$RFilename))
          save(file=GH$RFilename, GH)
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }




      if(K[Nclick]==match("SavePIX", BLABS, nomatch = NOLAB))
        {
          A = data.frame(WPX)
          A = A[WPX$onoff>=0, ]
          
           A = T12.pix(WPX)

           
    
           ireftrace = which.min(A$t1)
          
            UWFILEID = paste(sep="",
          formatC(A$yr[ireftrace], format="d", wid=4, flag="0"),
          formatC(A$mo[ireftrace], format="d", wid=2, flag="0"), 
          formatC(A$dom[ireftrace], format="d", wid=2, flag="0"), 
          formatC(A$hr[ireftrace], format="d", wid=2,  flag="0"), 
          formatC(A$mi[ireftrace], format="d", wid=2,flag="0"),
          formatC(floor(A$sec[ireftrace]), format="d", wid=2,flag="0")
          )

           fout = paste(sep=".", UWFILEID,"WPX")
           write.table(A, file =  fout)
        K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }







      
  ####################################### ################### ##################  
  ####################################### ################### ##################  

      if(K[Nclick]==match("LQUAKE", BLABS, nomatch = NOLAB))
        {
          
        ###  print(NH$pickfile)
          Apf =  NH$pickfile
          ###opix = vector()

          #############   make modifications to the pickfile here

          Apf$STAS$tag=WPX$tag[WPX$onoff>=0]
          Apf$STAS$name=WPX$name[WPX$onoff>=0]
          Apf$STAS$comp=WPX$comp[WPX$onoff>=0]
          Apf$STAS$c3=WPX$c3[WPX$onoff>=0]
          Apf$STAS$phase=WPX$phase[WPX$onoff>=0]
          Apf$STAS$sec=WPX$sec[WPX$onoff>=0]
          Apf$STAS$err=WPX$err[WPX$onoff>=0]
          Apf$STAS$pol=WPX$pol[WPX$onoff>=0]
          Apf$STAS$flg=WPX$flg[WPX$onoff>=0]
          Apf$STAS$res=WPX$res[WPX$onoff>=0]
          
#############   
          Lc  =  length(Apf$comments)
          Apf$comments[Lc+1] = paste("Repicked", date())
          USERNAME=Sys.getenv("USERNAME")
          HOST=Sys.getenv("HOST")
          Apf$comments[Lc+2] = paste(sep=" ", "BY:",USERNAME, HOST )

          
##########   next add in pick tokens that are new

          #########  make a backup o fh old pickfile:
          
          oldpf = paste(sep="", Apf$filename, "OLD")
          
          system(paste(sep=" ", "/bin/cp " , Apf$filename, oldpf))
         ##### output=paste(sep=".", Apf$filename, "temp")
          output=Apf$filename
          writeUWpickfile(Apf, output=output)

          setLQUAKE(NH$velfile, NH$stafile, fout="setup.lquake.ZZZ")
          
          system(paste("lquake -f setup.lquake.ZZZ" , output))
          P = getpfile(output)
          WPX=uwpfile2ypx(P)
          NH$pickfile = P
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }


      
       ####################################### ################### ##################  
  ####################################### ################### ##################  

        ################### 
      if(K[Nclick]==match("PickWin", BLABS, nomatch = NOLAB))
        {
          kix = legitpix(sel, zloc, zenclick)
          ypick =  kix$ypick
          ppick = kix$ppick
                
          if(length(ppick)>0)
            {
            
              ipick = sel[ypick]

              ipick = ipick[length(ipick)]
              
             ## cat(paste(sep=" ", ypick, ipick), sep="\n")
             ## print(ipick)
              ##
              
              ma = which(!is.na(match( NH$STNS, NH$STNS[ipick])))

              
              ##########   sort so Vertical is on top and then North and East
              acomp  = NH$COMPS[ma]
              icomp = rep(0, length(acomp))
              icomp[acomp=="V"] = 1
              icomp[acomp=="N"] = 2
              icomp[acomp=="E"] = 3

              ma = ma[order(icomp)]

              
####  print(cbind(NH$STNS[ma], NH$COMPS[ma]))

              
              if(is.null(Pickdev))
                {
               ####   X11(width = 12, height = 7)
                  screens(2)
                  devl = dev.list()
                  iw =  which(MAINdev!=devl)
                  
                  Pickdev = devl[iw[1]]
                   dev.set(Pickdev)
                }
              else
                {
                 #### devl = dev.list()
                ####  jsc = 2-length(devl)
                ####  if(jsc>0) { X11(width = 12, height = 7); Pickdev = dev.cur() }
                  dev.set(Pickdev)
                }


              

              if(zenclick>2)
                {

                  pickwin = range( c(zloc$x[(zenclick-1)], zloc$x[(zenclick-2)]))
                  
                }
              else
                {
                  pickwin = WIN

                }
              
              
              PLAB=c( "Ppic", "Spic", "Apic",  "Pup", "Pdown", "Pnil", "AUTOP", "NOPIX", "EDIX", "REPIX")
              PICKLAB = c("DONE", "zoom out","zoom in", "refresh", "restore", "FILT", "UNFILT", "Pinfo", "WINFO")
              
              stit = NH$STNS[ma[1]]
              ##  SWP = selAPX(WPX,  NH$STNS[ma[1]], icomp=NULL )

              ##   print(data.frame(SWP))
              ##   SWP = rectifyAPX(SWP)
              ##
              ## print(SWP)

              
              newpicks = PICK.GEN(NH, APIX=WPX, sel=ma, WIN=pickwin, STDLAB=PICKLAB ,PADDLAB=PLAB, PHASE=1   ,SHOWONLY = FALSE, TIT=stit)

              if(length(newpicks)>1)
                {
                  if(!is.null(newpicks$WPX))
                    {
                      WPX = newpicks$WPX
                    }
                }
              ##  
              
              ##
####    print(cbind(WPX$name, WPX$comp, WPX$phase, WPX$onoff))
              NPX = length(WPX$name)
              print(paste(sep=' ', "DONE with PICKWIN", NPX))
              dev.set( MAINdev)

              
               YN = YNreplot()
           
              buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              ## PLOT.ALLPX(Torigin, STNS, COMPS, PHASE=PHASE, WPX, FORCE=forcepix)

              ## NPX = length(WPX$name)

              
            }
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
          
        }
 ########  source("PICK.R") ; save.image()
      
#############################################################################
       ################### 
      
      if(K[Nclick]==match("Ppic", BLABS, nomatch = NOLAB))
        {
              zappa = match(BLABS[K[Nclick]], PADDLAB)
              azap = PADDLAB[zappa]
              print(paste(sep=" ", "My PICKIN", azap, zappa))

              kix = legitpix(sel, zloc, zenclick)
              ypick =  kix$ypick
              ppick = kix$ppick
 
###   print(paste(sep=" " , "WIN=",sloc$x))
              
###        abline(v=ppick, col=4)
              
              ipick = sel[ypick]

              ipick = ipick[1]

            
               print(paste(sep=" ", "PICK=", NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick],
                      NH$info$mi[ipick], "sta=", NH$STNS[ipick], "comp=", NH$COMPS[ipick] ))

              m = match(STNS[ypick],UNIsta)
###  Upix[[m]]$x  = ppick
              
###   PPIX(list(x=zloc$x[zenclick-1], y=zloc$y[zenclick-1]), YN=NSEL, col=3, lab="P")
              jj = floor((zloc$y[zenclick-1])/du)
             
              if((zenclick==2))
                 {
                   asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-1]
                   err = 0.05
                 }
                 else
                 {
                   asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-2]
                   bsec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-1]
                   err =  abs(bsec-asec)
                 }

              ###########   this looks like a bug./....
               
              iseek = which(WPX$phase=="P" & WPX$name==NH$STNS[ipick] &  WPX$comp==NH$COMPS[ipick])
            ###  print(paste(sep=" ", "ISEEK",  iseek, length(iseek) ))
              
              if(length(iseek)==1)
                {
                  wNPX = iseek
                  
                  WPX$yr[wNPX]=NH$info$yr[ipick]
                  WPX$mo[wNPX]= NH$info$mo[ipick]
                  WPX$dom[wNPX]=NH$info$dom[ipick]
                  WPX$jd[wNPX]=NH$info$jd[ipick]
                  WPX$hr[wNPX]= NH$info$hr[ipick]
                  WPX$mi[wNPX]=NH$info$mi[ipick]
                   WPX$col[wNPX]=specpix.col[4]
                  WPX$sec[wNPX]=asec
                    WPX$err[wNPX]=err
                  WPX$onoff[wNPX] = 1 
                }
              else
                {
                  NPX = NPX+1
                  wNPX  = NPX
                 ####  tag = paste(sep=".",NH$STNS[ipick],  NH$OCOMPS[ipick])
                 ####  print(tag)
                  Nn = names(WPX)
                  WPX =rbind(WPX, rep(NA, length(Nn)))
  
                  #########   a 
                  WPX$tag[wNPX]=paste(sep=".",NH$STNS[ipick],  NH$OCOMPS[ipick])
                 
                  WPX$name[wNPX]=NH$STNS[ipick]
                  WPX$comp[wNPX]=NH$COMPS[ipick]
                  WPX$c3[wNPX]=NH$OCOMPS[ipick]
                  WPX$phase[wNPX]="P"
                  
                  WPX$yr[wNPX]=NH$info$yr[ipick]
                  WPX$mo[wNPX]= NH$info$mo[ipick]
                  WPX$dom[wNPX]=NH$info$dom[ipick]
                  WPX$jd[wNPX]=NH$info$jd[ipick]
                  WPX$hr[wNPX]= NH$info$hr[ipick]
                  WPX$mi[wNPX]=NH$info$mi[ipick]
                  WPX$sec[wNPX]=asec
                  WPX$col[wNPX]=specpix.col[4]
                  WPX$onoff[wNPX] = 1 
                  WPX$err[wNPX]=err
                  WPX$flg[wNPX] = 0
                  WPX$res[wNPX] = NA

                }
              YN = YNreplot()
             
              buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              
              NADDPIX = 3
###

              K[Nclick] = 0
              zloc = list(x=NULL, y=NULL) 
            }

#########################
#########################
#########################
#########################
      
      if(K[Nclick]==match("Spic", BLABS, nomatch = NOLAB))
        {
          zappa = match(BLABS[K[Nclick]], PADDLAB)
          azap = PADDLAB[zappa]
          print(paste(sep=" ", "My PICKIN", azap, zappa))
          
               kix = legitpix(sel, zloc, zenclick)
      ypick =  kix$ypick
      ppick = kix$ppick
  
###   print(paste(sep=" " , "WIN=",sloc$x))
          
###        abline(v=ppick, col=4)
          
          
          ipick = sel[ypick]
          ipick = ipick[1]
        
          

          print(paste(sep=" ", "PICK=", NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick],
                      NH$info$mi[ipick], "sta=", NH$STNS[ipick], "comp=", NH$COMPS[ipick] ))
          
          m = match(STNS[ypick],UNIsta)
###  Upix[[m]]$x  = ppick
          
###   PPIX(list(x=zloc$x[zenclick-1], y=zloc$y[zenclick-1]), YN=NSEL, col=3, lab="P")
          jj = floor((zloc$y[zenclick-1])/du)
          
          if((zenclick==2))
            {
              asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-1]
              err = 0.05
            }
          else
            {
              asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-2]
              bsec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-1]
              err =  abs(bsec-asec)
            }
          
###########   this looks like a bug./....
          
          iseek = which(WPX$phase=="S" & WPX$name==NH$STNS[ipick] &  WPX$comp==NH$COMPS[ipick])
          #### print(paste(sep=" ", "ISEEK",  iseek, length(iseek)))

          
          if(length(iseek)==1)
            {
              wNPX = iseek
              
              WPX$yr[wNPX]=NH$info$yr[ipick]
              WPX$mo[wNPX]= NH$info$mo[ipick]
              WPX$dom[wNPX]=NH$info$dom[ipick]
              WPX$jd[wNPX]=NH$info$jd[ipick]
              WPX$hr[wNPX]= NH$info$hr[ipick]
              WPX$mi[wNPX]=NH$info$mi[ipick]
              WPX$sec[wNPX]=asec
                WPX$err[wNPX]=err
              WPX$col[wNPX]=specpix.col[2]
              WPX$onoff[wNPX] = 1 
            }
          else
            {
              NPX = NPX+1
              wNPX  = NPX
               ####stag = paste(sep=".",NH$STNS[ipick],  NH$OCOMPS[ipick])
                ####  print(stag)

              Nn = names(WPX)
              WPX =rbind(WPX, rep(NA, length(Nn)))
  

              
              WPX$tag[wNPX]=paste(sep=".",NH$STNS[ipick],  NH$OCOMPS[ipick])
              WPX$name[wNPX]=NH$STNS[ipick]
              WPX$comp[wNPX]=NH$COMPS[ipick]
              WPX$c3[wNPX]=NH$OCOMPS[ipick]
              WPX$phase[wNPX]="S"
              
              WPX$yr[wNPX]=NH$info$yr[ipick]
              WPX$mo[wNPX]= NH$info$mo[ipick]
              WPX$dom[wNPX]=NH$info$dom[ipick]
              WPX$jd[wNPX]=NH$info$jd[ipick]
              WPX$hr[wNPX]= NH$info$hr[ipick]
              WPX$mi[wNPX]=NH$info$mi[ipick]
              WPX$sec[wNPX]=asec
                WPX$err[wNPX]=err
              WPX$col[wNPX]=specpix.col[2]
              WPX$onoff[wNPX] = 1 
                                WPX$flg[wNPX] = 0
                  WPX$res[wNPX] = NA

            }
          YN = YNreplot()
             
              buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              
          NADDPIX = 3
###
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }

  
      if(K[Nclick]==match("Apic", BLABS, nomatch = NOLAB))
        {
          zappa = match(BLABS[K[Nclick]], PADDLAB)
          azap = PADDLAB[zappa]
          print(paste(sep=" ", "My PICKIN", azap, zappa))

          kix = legitpix(sel, zloc, zenclick)
          ypick =  kix$ypick
          ppick = kix$ppick
          
###   print(paste(sep=" " , "WIN=",sloc$x))
          
###        abline(v=ppick, col=4)
          
         
          ipick = sel[ypick]
          ipick = ipick[1]
         

          
          print(paste(sep=" ", "PICK=", NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick],
                      NH$info$mi[ipick], "sta=", NH$STNS[ipick], "comp=", NH$COMPS[ipick] ))
          
          m = match(STNS[ypick],UNIsta)
###  Upix[[m]]$x  = ppick
          
###   PPIX(list(x=zloc$x[zenclick-1], y=zloc$y[zenclick-1]), YN=NSEL, col=3, lab="P")
          jj = floor((zloc$y[zenclick-1])/du)
          
              if((zenclick==2))
                {
                  asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-1]
                  err = 0.05
                }
              else
                {
                  asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-2]
                  bsec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-1]
                  err =  abs(bsec-asec)
                 }
          
###########   this looks like a bug./....
          
          iseek = which(WPX$phase=="A" & WPX$name==NH$STNS[ipick] &  WPX$comp==NH$COMPS[ipick])
          
          if(length(iseek)==1)
                {
                  wNPX = iseek
                  
                  WPX$yr[wNPX]=NH$info$yr[ipick]
                  WPX$mo[wNPX]= NH$info$mo[ipick]
                  WPX$dom[wNPX]=NH$info$dom[ipick]
                  WPX$jd[wNPX]=NH$info$jd[ipick]
                  WPX$hr[wNPX]= NH$info$hr[ipick]
                  WPX$mi[wNPX]=NH$info$mi[ipick]
                  WPX$sec[wNPX]=asec
                  WPX$err[wNPX]=err
                  WPX$onoff[wNPX] = 1 
                }
              else
                {
                  NPX = NPX+1
                  wNPX  = NPX

                  Nn = names(WPX)
                  WPX =rbind(WPX, rep(NA, length(Nn)))
  

                  
                  WPX$tag[wNPX]=paste(sep=".",NH$STNS[ipick],  NH$COMPS[ipick])
                  WPX$name[wNPX]=NH$STNS[ipick]
                  WPX$comp[wNPX]=NH$COMPS[ipick]
                  WPX$c3[wNPX]=NH$OCOMPS[ipick]
                  WPX$phase[wNPX]="A"
                  
                  WPX$yr[wNPX]=NH$info$yr[ipick]
                  WPX$mo[wNPX]= NH$info$mo[ipick]
                  WPX$dom[wNPX]=NH$info$dom[ipick]
                  WPX$jd[wNPX]=NH$info$jd[ipick]
                  WPX$hr[wNPX]= NH$info$hr[ipick]
                  WPX$mi[wNPX]=NH$info$mi[ipick]
                  WPX$sec[wNPX]=asec
                  WPX$err[wNPX]=err
                  WPX$col[wNPX]=specpix.col[3]
                  WPX$onoff[wNPX] = 1
                  WPX$flg[wNPX] = 0
                  WPX$res[wNPX] = NA
                  
                  
                }
              YN = YNreplot()
         
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              
              NADDPIX = 3
###
          K[Nclick] = 0

          
              zloc = list(x=NULL, y=NULL) 
            }
      
########  source("PICK.R") ; save.image()
#############################################################################
################### 
    ########  source("PICK.R") ; save.image()
    
      if(K[Nclick]==match("Pup", BLABS, nomatch = NOLAB))
        {
              zappa = match(BLABS[K[Nclick]], PADDLAB)
              azap = PADDLAB[zappa]
              print(paste(sep=" ", "My PICK up", azap, zappa))

              kix = legitpix(sel, zloc, zenclick)
              ypick =  kix$ypick
              ppick = kix$ppick
              
              
              
              ipick = sel[ypick]
              
               print(paste(sep=" ", "PICK=", NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick],
                      NH$info$mi[ipick], "sta=", NH$STNS[ipick], "comp=", NH$COMPS[ipick] ))

              m = match(STNS[ypick],UNIsta)

              jj = floor((zloc$y[zenclick-1])/du)
             
              iseek = which(WPX$phase=="P" & WPX$name==NH$STNS[ipick] &  WPX$comp==NH$COMPS[ipick])
            ####  print(paste(sep=" ", "ISEEK",  iseek, length(iseek) ))
              
              if(length(iseek)==1)
                {
                  wNPX = iseek
                  
                  WPX$pol[wNPX]="U"
                  
                }
              YN = YNreplot()
              
              buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              
              NADDPIX = 3
              K[Nclick] = 0
              zloc = list(x=NULL, y=NULL) 
            }

################### 
    ########  source("PICK.R") ; save.image()
    
      if(K[Nclick]==match("Pdown", BLABS, nomatch = NOLAB))
        {
              zappa = match(BLABS[K[Nclick]], PADDLAB)
              azap = PADDLAB[zappa]
              print(paste(sep=" ", "My PICK down", azap, zappa))
              kix = legitpix(sel, zloc, zenclick)
              ypick =  kix$ypick
              ppick = kix$ppick
              
              
              ipick = sel[ypick]
              
               print(paste(sep=" ", "PICK=", NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick],
                      NH$info$mi[ipick], "sta=", NH$STNS[ipick], "comp=", NH$COMPS[ipick] ))

              m = match(STNS[ypick],UNIsta)

              jj = floor((zloc$y[zenclick-1])/du)
             
              iseek = which(WPX$phase=="P" & WPX$name==NH$STNS[ipick] &  WPX$comp==NH$COMPS[ipick])
            ####  print(paste(sep=" ", "ISEEK",  iseek, length(iseek) ))
              
              if(length(iseek)==1)
                {
                  wNPX = iseek
                  
                  WPX$pol[wNPX]="D"
                  
                }
              YN = YNreplot()
            
              buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              
              NADDPIX = 3
              K[Nclick] = 0
              zloc = list(x=NULL, y=NULL) 
            }

################### 
    ########  source("PICK.R") ; save.image()
    
      if(K[Nclick]==match("Pnil", BLABS, nomatch = NOLAB))
        {
          zappa = match(BLABS[K[Nclick]], PADDLAB)
              azap = PADDLAB[zappa]
              print(paste(sep=" ", "My PICK down", azap, zappa))
          kix = legitpix(sel, zloc, zenclick)
          ypick =  kix$ypick
          ppick = kix$ppick
          
            
              ipick = sel[ypick]
              
               print(paste(sep=" ", "PICK=", NH$info$yr[ipick], NH$info$jd[ipick], NH$info$hr[ipick],
                      NH$info$mi[ipick], "sta=", NH$STNS[ipick], "comp=", NH$COMPS[ipick] ))

              m = match(STNS[ypick],UNIsta)

              jj = floor((zloc$y[zenclick-1])/du)
             
              iseek = which(WPX$phase=="P" & WPX$name==NH$STNS[ipick] &  WPX$comp==NH$COMPS[ipick])
            ####  print(paste(sep=" ", "ISEEK",  iseek, length(iseek) ))
              
              if(length(iseek)==1)
                {
                  wNPX = iseek
                  
                  WPX$pol[wNPX]=NA
                  
                }
              YN = YNreplot()
             
              buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
              
              NADDPIX = 3
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
            }
      ################### 
    ########  
      if(K[Nclick]==match("YPIX", BLABS, nomatch = NOLAB))
        {

          if(zenclick>=2)
            {
          zappa = match(BLABS[K[Nclick]], BLABS)
          col = colpix[which(pnos=="YPIX")]
          kix = legitpix(sel, zloc, zenclick)
          ypick =  kix$ypick
          ppick = kix$ppick

          ############   proceed only if have legitimate picks
          if(length(ypick)>0)
            {
              
              azap = "YPIX"
              kzap = "Y"
              
              
              ipick = sel[ypick]
              
              
#### print(paste(sep=" ", "DUMP YPIX", zappa, col, azap, kzap , ppick , ypick,ipick)) 
              
              for(iz in 1:length(ypick))
                {
                  
                  NPX = NPX+1
                  Nn = names(WPX)
                  WPX =rbind(WPX, rep(NA, length(Nn)))
                  
                  i1 = ipick[iz]
                  i2 = ypick[iz]

                  ycol = colpix[zappa]
                  if(is.na(ycol)) { ycol = rgb(0,0,1) }
                  err = NA
                  WPX =  pickhandler(i1=i1, ppick=ppick[iz], kzap=kzap, err=NA, ycol=ycol, NPX=NPX, WPX=WPX, NH=NH)

                  
                  NADDPIX = NADDPIX+1
                  ## 

                  ## 
                }
              PLOT.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE, FORCE=forcepix, cex=pcex)
              
            }
        }
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
         
        }
#############################################################################
       ################### 
      if(K[Nclick]==match("WPIX", BLABS, nomatch = NOLAB))
        {
##############  window picks - these must be done in pairs.
          ###   the first click determines the station comp,
          ########    the second the window length
          if(zenclick>=2)
            {
          zappa = match(BLABS[K[Nclick]], BLABS)
          col = colpix[which(pnos=="WPIX")]
          
          azap = "WPIX"
          kzap = substr(azap, 1, 1)


          kix = legitpix(sel, zloc, zenclick)
          ypick =  kix$ypick
          ppick = kix$ppick

          
          ############   proceed only if have legitimate picks
          if(length(ypick)>0)
            {
              ipick = sel[ypick]
              
####### print(ppick)
#######  print(ypick)
#######  print(ipick)
### print(paste(' ', "Nclick=", Nclick))
#### print(STNS)
#### print(COMPS)

              print(paste(sep=" ", "zappa" ,zappa, col, azap, kzap , ppick , ypick,ipick))

######### WPIX must come in pairs, for a pick plus a duration

              npick = length(ypick)
      #####        
       #####       if( (npick%% 2)!=0 & npick>2 )
      #####         {
      #####           npick = npick - 1
       #####       }


              
                  for(iz in seq(from=1, to=npick-1, by=2))
                    {

                      i1 = ipick[iz]
                      i2 = ypick[iz]

                      asec = NH$info$sec[i1]+NH$info$msec[i1]/1000+NH$info$t1[i1]-NH$info$off[i1]+ppick[iz]

                      if(npick<2)
                        {
                          bsec = asec+5
                        }
                      else
                        {

                          iz1 = ipick[iz+1]
                          bsec = NH$info$sec[iz1]+NH$info$msec[iz1]/1000+NH$info$t1[iz1]-NH$info$off[iz1]+ppick[iz+1]

                        }
####   print("############################")
####  print(paste(' ', "wpix diag", NPX, iz, i1, i2, STNS[i2], COMPS[i2], asec))
                      print(paste(' ', "wpix diag", NPX, iz, i1, i2, STNS[i2], COMPS[i2], asec, bsec))

                      dur = diff(c(asec, bsec) )
                      print(paste(' ', "wpix diag", NPX, iz, i1, i2, STNS[i2], COMPS[i2], asec, bsec, dur))

                      
                      if(is.null(dur)) dur = 0
####  if(dur<=0) dur=1

                      
                      NPX = NPX+1
                      ycol = colpix[zappa]
                      if(is.na(ycol)) { ycol = rgb(0,0,1) }


                      Nn = names(WPX)
                      WPX =rbind(WPX, rep(NA, length(Nn)))
                      

                      WPX$tag[NPX]=paste(sep=".",NH$STNS[i1],  NH$COMPS[i1])
                      WPX$name[NPX]=NH$STNS[i1]
                      WPX$comp[NPX]=NH$COMPS[i1]
                      WPX$c3[NPX]=NH$OCOMPS[i1]
                      WPX$phase[NPX]=kzap
                      
                      WPX$err[NPX]=0.05
                      WPX$pol[NPX]=0
                      WPX$flg[NPX]=0
                      WPX$res[NPX]=dur
                      WPX$yr[NPX]=NH$info$yr[i1]
                      WPX$mo[NPX]= NH$info$mo[i1]
                      WPX$dom[NPX]=NH$info$dom[i1]
                      WPX$jd[NPX]=NH$info$jd[i1]
                      WPX$hr[NPX]=NH$info$hr[i1]
                      WPX$mi[NPX]=NH$info$mi[i1]
                      WPX$sec[NPX]=asec
                      WPX$col[NPX]=ycol
                      WPX$onoff[NPX] = 1 

                      
                      NADDPIX = NADDPIX+1
                      

                      
                      ##   
                    }
                
            }
          else
            {
              print("not enough legitimate picks, need at least 2 or more")

            }

        }
          zloc = list(x=NULL, y=NULL) 
          K[Nclick] = 0
        }






       ################### 
      if(K[Nclick]==match("RMS", BLABS, nomatch = NOLAB))
        {   ###  RMS button
##############  rms window picks - these must be done in pairs.
###   the first click determines the station comp,
########    the second the window length
          if(zenclick>=2)
            {
              zappa = match(BLABS[K[Nclick]], BLABS)
              col = colpix[which(pnos=="WPIX")]
              
              azap = "WPIX"
              kzap = substr(azap, 1, 1)


              kix = legitpix(sel, zloc, zenclick)
              ypick =  kix$ypick
              ppick = kix$ppick

              myinfo = list(yr=NH$info$yr, jd=NH$info$jd, hr=NH$info$hr, mi=NH$info$mi, sec=rep(0, times=length(NH$info$mi)))

              if(length(ypick)>0)
                {   ############   length(ypick) proceed only if have legitimate picks

                  ipick = sel[ypick]
                  
######### WPIX must come in pairs, for a pick plus a duration

                  npick = length(ypick)
#####        
                  
                  pairseq = seq(from=1, to=npick-1, by=2)
                  
#####    Output1 = vector(length=length(pairseq))
                  Output2 = vector(length=length(pairseq))
                  for(iz in  pairseq)
                    {   ###############   loop over pairs of picks

                      i1 = ipick[iz]
                      i2 = ypick[iz]

                      ################  this is the time in sec from the beginning of the trace
                      asec = NH$info$sec[i1]+NH$info$msec[i1]/1000+NH$info$t1[i1]-NH$info$off[i1]+ppick[iz]
                      if(npick<2)
                        {
                          bsec = asec+5
                        }
                      else
                        {

                          iz1 = ipick[iz+1]
                          bsec = NH$info$sec[iz1]+NH$info$msec[iz1]/1000+NH$info$t1[iz1]-NH$info$off[iz1]+ppick[iz+1]

                        }

                      
                      rsig1 = NH$JSTR[[i1]]
                      
                      t1 = seq(from=0, length=length(rsig1), by=NH$dt[i1])

                      which.time = which( t1>ppick[iz]  & t1< ppick[iz+1] )
                      rwhich = range(which.time)
                      
                      rsig  = rsig1[ which.time ]
                      
                      rsig = rsig-mean(rsig)
                        
                      rms = sqrt( mean( rsig^2 ))

                      cat(paste(sep=" ", "#########", iz, i1, format(ppick[iz]), format(ppick[iz+1]),
                                format(asec) , format(bsec), length(rsig), format(rms) ), sep="\n" )

                      Output2[iz] = paste(sep=" ",
                               NH$STNS[i1],
                               NH$COMPS[i1] ,
                               myinfo$yr[i1],
                               myinfo$jd[i1],
                               myinfo$hr[i1],
                               myinfo$mi[i1],
                               format(asec),
                               format(bsec),
                               format(rms))
                      
                      
                     ####### if(FALSE)
                     #######   {
                     #######     cat(paste(sep=" ","#########   To check this calculation, try this:"), sep="\n" )
                     #######     cat(paste(sep=" ", "gsel=", i1), sep="\n")
                     #######     cat(paste(sep=" ", "pXindex=c(", rwhich[1], ",", rwhich[2], ")"), sep="\n")
                     #######     cat(paste(sep=" ", "TEMPsig1=GH$JSTR[[gsel]]"), sep="\n")
                     #######     cat(paste(sep=" ", "TEMPsig1=TEMPsig1[pXindex[1]:pXindex[2]]"), sep="\n")
                     #######     cat(paste(sep=" ", "TEMPsig1=TEMPsig1-mean(TEMPsig1)"), sep="\n")
                     #######     cat(paste(sep=" ", "test=sqrt(mean(TEMPsig1^2))"), sep="\n")
                     #######     cat(paste(sep=" ", "pwin=c(", ppick[iz], ",", ppick[iz+1], ")"), sep="\n")
                     #######   }
                                       
                      dur = diff(c(asec, bsec) )
                 ###     print(paste(' ', "wpix diag", NPX, iz, i1, i2, STNS[i2], COMPS[i2], asec, bsec, dur))

                      
                      if(is.null(dur)) dur = 0
####  if(dur<=0) dur=1

                      
                      NPX = NPX+1
                      ycol = colpix[zappa]
                      if(is.na(ycol)) { ycol = rgb(0,0,1) }


                      Nn = names(WPX)
                      WPX =rbind(WPX, rep(NA, length(Nn)))
                      

                      WPX$tag[NPX]=paste(sep=".",NH$STNS[i1],  NH$COMPS[i1])
                      WPX$name[NPX]=NH$STNS[i1]
                      WPX$comp[NPX]=NH$COMPS[i1]
                      WPX$c3[NPX]=NH$OCOMPS[i1]
                      WPX$phase[NPX]=kzap
                      
                      WPX$err[NPX]=0.05
                      WPX$pol[NPX]=0
                      WPX$flg[NPX]=0
                      WPX$res[NPX]=dur
                      WPX$yr[NPX]=NH$info$yr[i1]
                      WPX$mo[NPX]= NH$info$mo[i1]
                      WPX$dom[NPX]=NH$info$dom[i1]
                      WPX$jd[NPX]=NH$info$jd[i1]
                      WPX$hr[NPX]=NH$info$hr[i1]
                      WPX$mi[NPX]=NH$info$mi[i1]
                      WPX$sec[NPX]=asec
                      WPX$col[NPX]=ycol
                      WPX$onoff[NPX] = 1 

                      
                      NADDPIX = NADDPIX+1
                      
                      ##   
                    }


                  PLOT.ALLPX(Torigin, STNS, COMPS, WPX,  FILL=FALSE,  PHASE="V", FORCE=FALSE, cex=pcex)

                  #######################   done with loop:  print results
                  cat("############", sep="\n")
                  cat( "OUTrms =scan(file=\"\", what=list(stn=\"\", comp=\"\", yr=0, jd=0, hr=0, mi=0, t1=0, t2=0, rms=0))", sep="\n" ) 
                  for(iz in  pairseq)
                    {
                      cat(Output2[iz], sep="\n")
                    }
                  cat("\n" )
                  cat("######", sep="\n")
                }
              else
                {
                  print("not enough legitimate picks, need at least 2 or more")

                }

            }
          zloc = list(x=NULL, y=NULL) 
          K[Nclick] = 0
        }

#############################################################################
       ################### 
      if(K[Nclick]==match("EDIX", BLABS, nomatch = NOLAB))
        {

          if(zenclick>=2)
            {
          PTOL = 0.1
          zappa = match(BLABS[K[Nclick]], PADDLAB)
          col = colpix[which(pnos=="EDIX")]
          
          azap = PADDLAB[zappa]
          kzap = substr(azap, 1, 1)
           kix = legitpix(sel, zloc, zenclick)
      ypick =  kix$ypick
      ppick = kix$ppick
     
         
              ipick = sel[ypick]
         
         ####################################  this is not finished yet
         ###### ma = which(!is.na(match( NH$STNS, NH$STNS[ipick])))
         ###### print(cbind(NH$STNS[ma], NH$COMPS[ma]))

          asec = NH$info$sec[ipick]+NH$info$msec[ipick]/1000+NH$info$t1[ipick]-NH$info$off[ipick]+ppick[zenclick-1]

           iseek = which(WPX$name==NH$STNS[ipick] &  WPX$comp==NH$COMPS[ipick])
          if(length(iseek)<1)
            {
              print("No Match")
              
            }
          else
            {
              wNPX = iseek
             
              bsec=secdif( NH$info$jd[ipick], NH$info$hr[ipick], NH$info$mi[ipick],    asec,
                WPX$jd[wNPX],WPX$hr[wNPX],WPX$mi[wNPX],  WPX$sec[wNPX])
              wtol = abs(bsec)<PTOL

          #########     print(paste( sep=" ", "Match",  wNPX, PTOL, any(wtol) ))
          #########      print(abs(bsec))
              
              if(any(wtol))
                {
                  ichange = wNPX[wtol]

              #########     print(ichange)
                   WPX$onoff[ichange] = -1

                }

            }
      YN = YNreplot()
         
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
        }
          K[Nclick] = 0
          zloc = list(x=NULL, y=NULL) 
        }
#############################################################################
      
      ###################   generic  PICK   ###########################      

   if(Nclick>0 & length(grep("pik", BLABS))>0  )
     {
###   restrict search to only labels that have PIX in them
       if(zenclick>=2)
         {
           glabs = grep("pik", BLABS[K[Nclick]])
           
           if(length(glabs)>0)
             {
               ##    zappa = match(BLABS[K[Nclick]], PADDLAB)
               
               azap = BLABS[K[Nclick]]
               zappa=match(azap, PADDLAB)
               
               kzap = substr(azap, 1, nchar(azap)-3 )
               print(paste(sep=" ", "My PICKIN", azap, kzap,zappa ))

               kix = legitpix(sel, zloc, zenclick)
               ypick =  kix$ypick
               ppick = kix$ppick
               
               ipick = sel[ypick]
               
               for(iz in 1:(zenclick-1))
                 {
                   
                   NPX = NPX+1
                   Nn = names(WPX)
                   WPX =rbind(WPX, rep(NA, length(Nn)))
                   i1 = ipick[iz]
                   ycol = colpix[zappa]
                   if(is.na(ycol)) { ycol = rgb(0,0,1) }
                   err = NA
                   WPX =  pickhandler(i1=i1, ppick=ppick[iz], kzap=kzap, err=NA, ycol=ycol, NPX=NPX, WPX=WPX, NH=NH)
                   NADDPIX = NADDPIX+1
                   ## 
                   
                   ## 
                 }
               
               PLOT.ALLPX(Torigin, STNS, COMPS, WPX, PHASE=PHASE, FORCE=forcepix, cex=pcex)
               
#### print(paste(" ", azap, kzap))
              
               K[Nclick] = 0
               zloc = list(x=NULL, y=NULL) 
               
          
             }
         
         }
      
       
     }
###################   REMOVE  PICKs   ###########################      
      
      
      if(K[Nclick]==match("NOPIX", BLABS, nomatch = NOLAB))
        {

        #######  ONPX = NPX
       #######   OWPX = WPX
          
        ######  NPX = 0
          
          
          WPX$onoff = rep(-1, length(WPX$onoff))
          YN = YNreplot()
          
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)

          
        }
###################   return  PICKs   ###########################      
  

       if(K[Nclick]==match("REPIX", BLABS, nomatch = NOLAB))
        {
        #######  NPX = ONPX
       #######   WPX = OWPX
          WPX$onoff[WPX$onoff==(-1)] = 0
          YN = YNreplot()
         
          buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)

          zloc = list(x=NULL, y=NULL) 
        }
#########################################################################  
#########################################################################  
           
       if(K[Nclick] == match("ADDBUTTS", BLABS, nomatch = NOLAB))
          {
            if(!exists("buthome"))
            { buthome = "/home/lees/Progs/R_stuff" }
            buttfile = paste(sep='/', buthome, "MYPICKBUTTS.R")
            print(paste("ADDING button defined in file:", buttfile))

            
            source(buttfile)
            
            BLABS = c(BLABS , tempbuttons)
            NLABS = length(BLABS)
            NOLAB = NLABS +1000
            ScaleFACT = 1
            ##  match("", BLABS)
            
            RETX =  NULL

            pnos = c( grep("PIX", BLABS), grep("pik", BLABS))
            
            colabs = rep(1,length(BLABS))
            colabs[pnos] = seq(from=2, length=length(pnos))
            colpix = seq(from=2, length=length(pnos))

          
            pchlabs = rep(4,length(BLABS))
            pchlabs[pnos] = seq(from=15, length=length(pnos))
            
            buttons = rowBUTTONS(BLABS, col=colabs, pch=pchlabs)
            
#######  source("MYBUTTS")
            zloc = list(x=NULL, y=NULL) 
          }
        
      if(any(K[Nclick] == match(tempbuttons, BLABS, nomatch = NOLAB)))
        {
###   print(BLABS[K[Nclick]])
          doMYBUTTS(butt=BLABS[K[Nclick]], click=zloc, x=NULL)
          zloc = list(x=NULL, y=NULL) 
        }
#########################################################################

     ##### doMYBUTTS(butt=BLABS[K[Nclick]], click=zloc, NH=NH, sel=sel, APIX=WPX)
#########################################################################  

      ####################  END BUTTON DEFINITIONS    ###########################      
            ###################   WRAP UP and PLOT AGAIN   ###########################      

###  NV = LabelBAR(BLABS)
### K = ValBAR(NV, zloc)

### print(paste(sep=" ", "K=",K))
      
    }

### PRET = list(TPIX=TPIX, xpix=xpix,ypixA=ypixA, ypixB=ypixB,cpixa= cpixa, cpixb=cpixb, cpixc=cpixc, colpix=colpix)
###  return(PRET)
  if(zenclick>2)
    {
      pwin = sort(c(zloc$x[zenclick-2], zloc$x[zenclick-1]))
    }
  else
    {
      pwin =  c( zloc$x[zenclick-2], zloc$x[zenclick-1])

    }
#### print(pwin)
#### print(WIN)

  
  PushI =  whichbutt(zloc ,buttons)
####  print(zloc)
####  print(sloc)
####  print(PushI)
  
  
  PushK=NULL
  if(length(PushI)>=1)
    {
      PushK=NULL
      if(any(PushI>0)) PushK =BLABS[PushI[PushI>0]]
    }
  
  
  but=BLABS[K[Nclick]]


  whirid = which( WPX$name==NA & WPX$comp==NA & WPX$phase==NA )
  WPX = WPX[-whirid, ]
  RETP = list(but=but, sloc=sloc, WPX=WPX, BRUNINFO=BRUNINFO, DETLINFO=DETLINFO,  mark=mark, PUSHED=PushK)
  
  
  if(!is.null(RETX))
    {
      invisible(RETX)
    }
  else
    {
      invisible(RETP)
    }
  
}


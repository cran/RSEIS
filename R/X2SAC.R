X2SAC<-function(nh, g)
  {
#####  BUTTONDOC:X2SAC:'data extraction '
    kix = legitpix(g$sel, g$zloc, g$zenclick)
    ypick =  kix$ypick
    ppick = kix$ppick
    
    if(length(ppick)>0)
      {

        ipick = g$sel[ypick]

        ipick = ipick[length(ipick)]
        
        ## message(paste(sep=" ", ypick, ipick) )
        ## message(ipick)
        ##
       if(g$zenclick>2)
              {
                pickwin = range( c(g$zloc$x[(g$zenclick-1)], g$zloc$x[(g$zenclick-2)]))
                
              }
            else
              {
                pickwin = g$WIN

              }
          
        jh =  WINGH(nh,  WIN = pickwin )


        pstamp = Zdate(jh$info)

        message(paste("Creating SAC files in dir:", pstamp[1]), sep="\n")
        
        rseis2sac(jh,   path = pstamp[1], BIGLONG=FALSE )


        
         g$zloc = list(x=NULL, y=NULL) 
        
        g$action="donothing"
        invisible(list(RETX =jh , global.vars=g))
      
      }
else
      {
        warning("X_R WARNING: no window or trace has been selected:")
        RETX=NULL
        g$zloc = list(x=NULL, y=NULL) 
        
        g$action="donothing"
        invisible(list(global.vars=g))
        
      }

  }


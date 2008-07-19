`PMOT.drive` <-
function(temp,  dt, pmolabs=c("Vertical", "North", "East"), STAMP="", baz=0 )
  {

    if(missing(STAMP)) { STAMP = " " }
    if(missing(baz)) { baz=0 }
    if(missing(pmolabs)) {pmolabs=c("Vertical", "North", "East")  }
    
    TPALS = c("rainbow", "topo.colors", "terrain.colors", "JGRAY", "tomo.colors")
    APALS = c("rainbow", "topo", "terrain", "JGRAY", "tomo")
    ADDBUTS = c("More" )
  
    rotlabs=c("Vertical", "Radial", "Transvers")
    vnelabs=c("Vertical", "North", "East")
    
    labs = c("DONE", "Angles", "PTS", "LOCS", "Postscript", "ROTATE", APALS, ADDBUTS )
    NLABS = length(labs)
    NOLAB = NLABS +1000
###  FUN = match.fun(TPALS[1])
     pal = Gcols(plow=0, phi=0,  N=100, pal=TPALS[1])
    scale.def = 0
   
    colabs = c(rep(1,2) , rep(2, length(APALS) ), rep(4,length(ADDBUTS) ))
    pchlabs = c(rep(1,2) , rep(2, length(APALS) ), rep(4,length(ADDBUTS) ))
 
    gridon = FALSE
 
  
    NSEL = 1

    ROTATEFLAG = 0

    atemp = temp

    if(baz!=0)
      {
        rbaz = grotseis(baz, flip=FALSE)
        btemp  = atemp  %*%  rbaz
      }
    else
      {
         btemp = atemp
      }
    

    
  ###  X11()
###  
    
    sx = complex.hodo(temp, dt=dt, labs=pmolabs, COL=pal, STAMP=STAMP)

    
    buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
      zloc = list(x=NULL, y=NULL)
    sloc = zloc

    
    while(TRUE)
      {

  iloc = ilocator(1, COL=rgb(1,0.8, 0.8), NUM=FALSE , YN=1, style=0)
           Nclick = length(iloc$x)
           
          if(Nclick>0)
            {
              zloc  = list(x=c(zloc$x,iloc$x), y=c(zloc$y, iloc$y))
              zenclick = length(zloc$x)
              K =  whichbutt(iloc ,buttons)
              sloc = zloc
            }
          else
            {
              Nclick = 0
              K = 0
              buttons = rowBUTTONS(labs, col=rep(grey(.8), length(labs)), pch=rep("NULL", length(labs)))
              title("Return to Calling Program")
              break;
            }
     


        
        if(K[Nclick] == match("DONE", labs, nomatch = NOLAB))
          {
            break;
          }
        if(zenclick == 1 &  K[Nclick]==0 )
          {
            ###  replot
            ###print(K[Nclick])
            
           sx = complex.hodo(temp, dt=dt, labs=pmolabs, COL=pal, STAMP=STAMP)
            buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
           zloc = list(x=NULL, y=NULL) 
           
          }

      ####################   POSTSCRIPT  ##################
      
        if(K[Nclick] == match("Postscript", labs, nomatch = NOLAB))
        {

          print("Start postscript plot.ts")
          plfname = local.file("pmot","eps")
          jdev = dev.cur()
          jpostscript("pmot")
          sx = complex.hodo(temp,  dt=dt, labs=pmolabs, COL=pal, STAMP=STAMP )
           print("Done creating postscript")
          dev.off()
          dev.set(jdev)
          zloc = list(x=NULL, y=NULL) 
        }

        if(K[Nclick] == match("ROTATE", labs, nomatch = NOLAB))
          {

            if(identical(ROTATEFLAG,1 ))
              {
                temp = atemp
                pmolabs=vnelabs
                ROTATEFLAG=0
              }
            else
              {
                temp = btemp
                pmolabs=rotlabs
                ROTATEFLAG=1
              }
            
            sx = complex.hodo(temp, dt=dt, labs=pmolabs, COL=pal, STAMP=STAMP)
            buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
            zloc = list(x=NULL, y=NULL) 
            

          }


        if(K[Nclick]==match("Angles", labs, nomatch = NOLAB) & (zenclick-1)>=2 )
          {
            
            
            print(zloc$x[1:(zenclick-1)])
            LN = length(zloc$x[1:(zenclick-1)])
            LN = 2*(floor(LN/2))
            ###  use only pairs of clicks
            
            sn1 = seq(from=1, to=LN-1, by=2)
            sn2 = sn1+1
            segments(zloc$x[sn1], zloc$y[sn1], zloc$x[sn2], zloc$y[sn2], col="black")
            print(STAMP)
            a1 = 180*atan2(zloc$y[sn2]-zloc$y[sn1], zloc$x[sn2]-zloc$x[sn1])/pi
            print(a1)
            zloc = list(x=NULL, y=NULL) 
          
          }

        if(K[Nclick]==match("LOCS", labs, nomatch = NOLAB))
          {
            print(zloc$x)
            LN = length(zloc$x[1:(zenclick-1)])
            sn1 = seq(from=1, to=LN, by=1)
            print(STAMP)

            plt1 = 1+floor(zloc$x[sn1])
           
            print(zloc$x[sn1])
            print(plt1)

            ids = idpoints.hodo(temp, sx, zloc$x[sn1], zloc$y[sn1])
            
            if(length(ids)>=2)
              {
                print(ids)

                t1 = ids[1]*dt
                t2t1 = dt*(ids[2]-ids[1])
                
                addpoints.hodo(temp, dt, sx,  flag=c(ids[1]:ids[2]) , pch=3, col="brown")
                
                femp = temp[c(ids[1]:ids[2]), ]
                covtem = var(femp)
                eg=eigen(covtem, symmetric = TRUE )
                
                arrows(.5, .5, .5+0.3*eg$vectors[3,1] , .5+0.3*eg$vectors[2,1])
                
                
                alpha=180*atan2(eg$vectors[2,1], eg$vectors[3,1])/pi
####az=90-alpha
                
                az = alpha
                inci=180*atan2(eg$vectors[1,1], sqrt(eg$vectors[2,1]^2+eg$vectors[3,1]^2))/pi
                rateig  = 1 - ((eg$values[2]+eg$values[3])/(2*eg$values[1]))
                
                plop = paste(sep="  ", STAMP, "Az=", format.default(az, width=6,digits=4, trim=FALSE),
                  "Inc=", format.default(inci, width=5, digits=4, trim=FALSE),
                  "Rat=", format.default(rateig, width=5,digits=4, trim=FALSE),
                  "T1=", format.default(t1, width=5,digits=4, trim=FALSE),
                  "T2-T1=", format.default(t2t1, width=5,digits=4, trim=FALSE)
                  )
                print(plop)
                text(0, 1.05, labels=plop, adj=0)
              }
             zloc = list(x=NULL, y=NULL) 
          }

        if(K[Nclick]==match("PTS", labs, nomatch = NOLAB))
          {
            addpoints.hodo(temp, dt, sx,  pch=3, col="brown")
             zloc = list(x=NULL, y=NULL) 
          }


           
        if(K[Nclick]==match("More", labs, nomatch = NOLAB))
          {
           
            sx = complex.hodo(temp, dt=dt, labs=pmolabs, COL=pal, STAMP=STAMP)
            buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
             zloc = list(x=NULL, y=NULL) 
          }
        if( length(which(K[Nclick] == match(APALS, labs, nomatch = NOLAB)))>0 )
          {
            J = match(labs[K[Nclick]] ,  APALS   )
            
            ##FUN = match.fun(TPALS[J])
            ##  pal = FUN(NCOL)
            pal = Gcols(plow=0, phi=0,  N=100, pal=TPALS[J])
            
            sx = complex.hodo(temp, dt=dt, labs=pmolabs, COL=pal, STAMP=STAMP)
            buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
             zloc = list(x=NULL, y=NULL) 
          }

      }

    print("DONE with PMOT")
    
    
  }


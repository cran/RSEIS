`drive.MTM` <-
function(a, f1=f1, f2=f2, len2=1024, COL=2, PLOT=FALSE, PADDLAB=NULL, GUI=TRUE)
  {

    ###  calculate and plot an MTM spectrum
    ####   a = list(y=ampv, dt=0.008)

    if(missing(PLOT)) { PLOT=TRUE }
    if(missing(GUI)) { GUI=TRUE }
    
    if(missing(f1)) { f1 = 0.01 }
    if(missing(f2)) { f2 = 10 }
    
    if(missing(PADDLAB)) { PADDLAB=NULL}
    if(missing(COL)) { COL=1:length(a$dt)}


    if(!is.list(a$y)) { a$y = list(y=as.vector(a$y))  }

    
     
    
    alen = unlist(lapply(a$y,  FUN="length"))
    len = max(alen)
    
    if(missing(len2))
      {
        len2 = 2*next2(len)
      }
    if(len2<len)
      {
        len2 = 2*next2(len)
      }
    
    

    M = length(a$dt)
    amp = list()
    dof = list()
    Fv = list()
    
    i = 1

   ##### print(c(len2, a$dt[[i]], alen) )
    
    Mspec =   mtapspec(a$y[[i]],a$dt[[i]], klen=len2,  MTP=list(kind=1,nwin=5, npi=3,inorm=0)  )
    f=Mspec$freq
    
    amp[[i]] = Mspec$spec[1:length(f)]
    dof[[i]] = Mspec$dof[1:length(f)]
    Fv[[i]] = Mspec$Fv[1:length(f)]

    
   ##### plot(f , amp[[i]]); locator()
    
    if(M>1)
      {
        for(i in 2:M)
          {
            Mspec =   mtapspec(a$y[[i]],a$dt[[i]], klen=len2,  MTP=list(kind=1,nwin=5, npi=3,inorm=0)  )      
            amp[[i]] = Mspec$spec[1:length(f)]
            dof[[i]] = Mspec$dof[1:length(f)]
            Fv[[i]] = Mspec$Fv[1:length(f)]
            
            
           ##### plot(f , amp[[i]]); locator()
          }
      }


 #######  source("drivers.R"); save.image()
    
    ma = amp;
    flag = f>=f1 & f <= f2;
    freqs = f[flag]
   ##### print(freqs)
    
    frange = range(freqs, na.rm = TRUE)
  #####  print(c(frange, length(flag), alen) )

    
    prange = range(amp[[1]][flag], na.rm = TRUE)
    for(i in 1:M)
      {
        amp[[i]]  = amp[[i]][ flag]
        dof[[i]] = dof[[i]][ flag]
        Fv[[i]] = Fv[[i]][ flag]
        prange = range(c(prange, range(unlist(amp[[i]]),na.rm = TRUE )))
        
        ##    abline(h=qf(ppoints/100, 2, 8))

      }
   ##### print(prange )
    
  displ = ma ;
  
  if(PLOT==TRUE)
    {
      stdlab = c("DONE", "REFRESH", "X-LOG", "Y-LOG", "VALS", "DOF", "F-Test", "AR", "Postscript", "POLYMOD", "STACK")
      labs = c(stdlab, PADDLAB)
      NLABS = length(labs)
      NOLAB = NLABS +1000
      colabs = rep(1, length=length(labs))
      pchlabs = rep(0,length(labs))
      plxy = ''

      mydof = NULL
      myFv = NULL
      
      plt.MTM0(frange,prange, plxy, M, freqs, amp , a, dof=mydof, Fv=myFv, COL=COL)
      
     
      if(GUI==FALSE) { return( list(len2=len2, f=f, f1=f1, f2=f2, displ=displ, ampsp=amp, flag=flag ) ) }
      
      u = par("usr")
      sloc = list(x=c(u[1],u[2]))
      
      
      buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
      
     

 iloc = ilocator(1, COL=rgb(1,0.8, 0.8), NUM=FALSE , YN=1, style=1)
  zloc = iloc
   Nclick = length(iloc$x)
  zenclick =  length(zloc$x)
  if(is.null(zloc$x)) { return(NULL) }
      K = whichbutt(iloc ,buttons)
      sloc = zloc
      
      plogx=''
      plogy=''
      
      
      
      while(TRUE)
        {
          
          if(K[Nclick] == match("DONE", labs, nomatch = NOLAB))
            {
              buttons = rowBUTTONS(labs, col=rep(grey(.8), length(labs)), pch=rep("NULL", length(labs)))
              title("Return to Calling Program")
        
              break;
            }
          
          if(K[Nclick] == match("REFRESH", labs, nomatch = NOLAB))
            {
             plt.MTM0(frange,prange, plxy, M, freqs, amp, a , dof=mydof, Fv=myFv, COL=COL)
              buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
             zloc = list(x=NULL, y=NULL) 
            }
          
          
          if(K[Nclick] == match("X-LOG", labs, nomatch = NOLAB))
            {
              if( (plogx=='x')==TRUE ) { plogx = '' }
              else { plogx = "x" }
              
              plxy = paste(sep='', plogx , plogy)
              plt.MTM0(frange,prange, plxy, M, freqs, amp , a, dof=mydof, Fv=myFv, COL=COL)
              buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
              zloc = list(x=NULL, y=NULL) 
              
            }
          if(K[Nclick] == match("Y-LOG", labs, nomatch = NOLAB))
            {
              if( (plogy=='y')==TRUE) { plogy = '' }
              else { plogy = "y" }


              plxy = paste(sep='', plogx , plogy)
              plt.MTM0(frange,prange, plxy, M, freqs, amp, a, dof=mydof, Fv=myFv, COL=COL )
              buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
              zloc = list(x=NULL, y=NULL) 
              
            }


          if(K[Nclick] == match("DOF", labs, nomatch = NOLAB))
            {
              if(is.null(mydof))
                {
                  mydof = dof
                }
              else
                {
                  mydof = NULL
                }
               plt.MTM0(frange,prange, plxy, M, freqs, amp, a, dof=mydof, Fv=myFv, COL=COL )
                buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
              zloc = list(x=NULL, y=NULL) 
          
            }

          if(K[Nclick] == match("F-Test", labs, nomatch = NOLAB))
            {
              if(is.null(myFv))
                {
                  myFv = Fv
                }
              else
                {
                  myFv = NULL
                }
              plt.MTM0(frange,prange, plxy, M, freqs, amp, a, dof=mydof, Fv=myFv, COL=COL )
                buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
              zloc = list(x=NULL, y=NULL) 
          
              
            }

         if(K[Nclick] == match("Postscript", labs, nomatch = NOLAB))
            {
              jpostscript("SPEC")
              plt.MTM0(frange,prange, plxy, M, freqs, amp, a, dof=mydof, Fv=myFv, COL=COL )
              dev.off()
              zloc = list(x=NULL, y=NULL) 
              
            }


          if(K[Nclick] == match("AR", labs, nomatch = NOLAB))
           {

              for(i in 1:M)
                {
                  squig = list(y=a$y[[i]], dt=a$dt[[i]])
                  ZIM = autoreg(squig , numf=length(freqs) , pord = 500, PLOT=FALSE,  f1=.01, f2=50)
                  why   = RESCALE(ZIM$amp , prange[1]  ,prange[2] , min(ZIM$amp, na.rm = TRUE)  , max(ZIM$amp, na.rm = TRUE)  )
                  lines(ZIM$freq, why, col=i)
             }
              zloc = list(x=NULL, y=NULL) 
             }

          #######  source("/home/lees/Progs/R_stuff/drivers.R"); save.image()

          if(K[Nclick] == match("POLYMOD", labs, nomatch = NOLAB))
            {

              jdev = dev.cur()
              
              if(zenclick>=3)
                {

                 ####  X11()
                  idev = dev.cur()
                  x1 = min(zloc$x[zenclick-2], zloc$x[zenclick-1])
                  x2 = max(zloc$x[zenclick-2], zloc$x[zenclick-1])
                  
              
                  jx = freqs[freqs>=x1&freqs<=x2]
                  NF = length(jx)
                  print(paste(sep=' ', x1, x2, "Hz", "N=", NF))
                  
                  for(i in 1:M)
                    {
                      why = amp[[i]]
                      ## 

                   ##    zy  = log10(why[freqs>=x1&freqs<=x2])
                   ##    zx = log10(jx)
                      zx =jx
                      zy  = why[freqs>=x1&freqs<=x2]
                      

                      PMOD = lm(zy ~ zx + I(zx^2) + I(zx^3) + I(zx^4) + I(zx^4))

                      nwize = sapply(zx, function(x) coef(PMOD) %*% x^(0:4))
                      
                   ####   plot(zx, zy, type='l', log='xy')
                      lines(zx, nwize, col=2)
                    ####  locator()
                      
                      
                    }

                ####  dev.off(which = idev)
                ####  dev.set(jdev)
                }
              zloc = list(x=NULL, y=NULL) 
              
              
            }
          

      
          if(K[Nclick] == match("STACK", labs, nomatch = NOLAB))
           {

             Jamp = rep(0, length(amp[[1]]))

             
              for(i in 1:M)
                {
                 Jamp = Jamp+amp[[i]]
                }


             M = M + 1
             amp[[M]] = Jamp/M
             COL[M] = 'black'
             
              plt.MTM0(frange,prange, plxy, M, freqs, amp, a , dof=mydof, Fv=myFv, COL=COL)
              buttons = rowBUTTONS(labs, col=colabs, pch=pchlabs)
             zloc = list(x=NULL, y=NULL) 
     
              zloc = list(x=NULL, y=NULL) 
             }

      
          
          
          if(K[Nclick] == match("VALS", labs, nomatch = NOLAB))
            {
              alabs = format.default(zloc$x[1:(zenclick-1)], digits=3)
              print(paste( paste( sep=' ',"Frequencies: efs=c(", paste(alabs, collapse=","), ")" )))
              ## abline(v=zloc$x[1:(zenclick-1)])
              ## mtext(labs, at=zloc$x[1:(zenclick-1)], side=3, line=0)
              zloc = list(x=NULL, y=NULL) 
        
            }
         
       

          iloc = ilocator(1, COL=rgb(1,0.8, 0.8), NUM=FALSE , YN=1, style=0)
          Nclick = length(iloc$x)

          if(Nclick>0)
            {
              zloc  = list(x=c(zloc$x,iloc$x), y=c(zloc$y, iloc$y))
              zenclick = length(zloc$x)
              K =  whichbutt(iloc ,buttons)
            }
          else
            {
              Nclick = 0

              K = 0

            }
     

          
          
        }
      
      
      
    }
  invisible( list(len2=len2, f=f, f1=f1, f2=f2, displ=displ, ampsp=amp, flag=flag ) )
}


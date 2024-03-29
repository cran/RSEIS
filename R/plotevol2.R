`plotevol2` <-
function(DEVOL, log=0,  fl=0, fh=10 , col=col, ylog=FALSE, ygrid=FALSE, AXE=c(1,2,3,4), CSCALE=FALSE, WUNITS="Volts", STAMP=NULL, STYLE="fft",  add=FALSE, IMAGE=TRUE, WIG=TRUE  )
  {
    if(missing(log)) { log = 0 }
    if(missing(fl)) { fl=DEVOL$wpars$fl}
    if(missing(fh)) { fh=DEVOL$wpars$fh}    
    if(missing(col)) { col=rainbow(50) }
    if(missing(ylog)) { ylog=FALSE }
    if(missing(ygrid)) { ygrid=FALSE }
    if(missing(AXE)) { AXE=c(1,2,3,4) }
    if(missing(CSCALE)) { CSCALE=TRUE  }
    if(missing(STAMP)) {  STAMP=NULL  }
    if(missing(WUNITS)) {   WUNITS=NULL  }
    if(missing(STYLE)) {  STYLE="fft"   }

    if(missing(add)) { add=FALSE  }
   if(missing(IMAGE)) { IMAGE=TRUE }
   if(missing(WIG)) { WIG=TRUE  }

   ## if(!exists(col))  {  col=rainbow(50) }
    if(length(col)<1) {  col=rainbow(50) }

    
    perc = 0.85
    a = DEVOL$sig
    dt = DEVOL$dt
    DSPEC = DEVOL$DSPEC
    numfreqs = DEVOL$numfreqs
    y = DEVOL$freqs
    x = DEVOL$tims
    

    yflag = (y>=fl&y<=fh)
    
    
    tim = dt*seq(0, length=length(a))
    
    
    ##   image(x,why,t(DSPEC[1:(numfreqs/2),]), add=TRUE, col = col,xlab='time', ylab='freq', axes=FALSE)
    
    ##   image(x,why,log10(t(DSPEC[1:(numfreqs/2),])), add=TRUE, col = col,xlab='time', ylab='freq', axes=FALSE)

   ##   message("in plotevol...")
    
    if(identical(STYLE, "fft"))
       {
         IMAT = t(DSPEC[1:(numfreqs/2),])
       }
       else
       {
         IMAT = t(DSPEC)
       }
    
    if(ylog==TRUE)
      {
        Yplmin = min(log10(y[yflag]))
        Yplmax = max(log10(y[yflag])) 
        why   = RPMG::RESCALE( log10(y[yflag]), 0 , perc ,Yplmin ,  Yplmax )
      }
    else
      {

        Yplmin =min(y[yflag], na.rm=TRUE)
        Yplmax =max(y[yflag], na.rm=TRUE)
        why   = RPMG::RESCALE( (y[yflag]), 0 , perc , Yplmin ,  Yplmax  )

      }

    subI = IMAT[ , yflag]
    
    if(log<=0){ ImPlot = subI; units="Amp" } 
    if(log==1){ ImPlot =     log10(subI) ; units="Log Amp"}
    if(log==2){ ImPlot = sqrt(subI) ; units="SQRT Amp"}
    if(log==3){ ImPlot = 20*log10(subI/ max(subI) ) ; units="DB"}
    if(log>3){ ImPlot = subI; units="Amp" }  


    ##   par(mfrow=c(1,1))
    par(xaxs='i', yaxs='i')

     if(add==FALSE)
      {
        plot(range(tim), c(0,1), axes=FALSE, type='n', xlab='', ylab='')
      }
    message(paste(collapse=' ', range( why)))

    if(IMAGE)
      {
    image(x,why,ImPlot, add = TRUE , col = col, xlab='time', ylab='freq', axes=FALSE)
  }

    if(add==TRUE) { return(NULL) }
    
    trace = RPMG::RESCALE( a, perc , 1.0  , min(a, na.rm=TRUE), max(a, na.rm=TRUE) )

     message(paste(sep=" ", "in plotevol...2  ADD=", add))
    message(paste(collapse=' ', range( trace)))
       
    if(WIG) lines(tim, trace)

    ##  sy = RPMG::RESCALE( a, perc , 1.0  , min(a), max(a) )
    Tdiff = max(tim, na.rm=TRUE)-min(tim, na.rm=TRUE)
    
    segments(max(tim)-Tdiff*.04-DEVOL$wpars$Ns*dt, perc+0.01, max(tim)-Tdiff*.04, perc+0.01, lwd=2)
    
                                        # axis(1)
                                        #  axis(3)
    
    xtix = pretty(x, n=10)
    xtix = xtix[xtix>=min(x)&xtix<max(x)]

    ### message(paste(sep=" ",min(x),  max(x), paste(xtix)))

  #   xtix = c(floor(min(x)),xtix,  floor(max(x)))
    axis(3,tck=.01,at=xtix,labels=FALSE)
    if(!is.na(match(3, AXE)))
      {

    mtext( side=3,    at=xtix, text=xtix, line=.5)
  }

    axis(1,tck=.01,at=xtix,labels=FALSE)
    mtext( side=1,    at=xtix, text=xtix, line=.25)

   
                                        #  title(xlab="Time, s")
    mtext(side=1, at=max(x), text="Time, s" , line=1.5, adj=1)

    if(ylog==TRUE)
      {
        axspec = pretty(log10(y[yflag]), n=10)
        axspec = axspec[axspec<=max(log10(y[yflag])) ]

        
        raxspec= RPMG::RESCALE( axspec, 0 , perc , min(log10(y[yflag]), na.rm=TRUE), max(log10(y[yflag]), na.rm=TRUE) )

        axspec = 10^(axspec[axspec<=max(log10(y[yflag])) ])
        axspec[ axspec<1] = 1/axspec[ axspec<1]
        
      }
    else
      {
        axspec = pretty(y[yflag], n=10)
        axspec = axspec[axspec<=max(y[yflag], na.rm=TRUE) ]
        raxspec= RPMG::RESCALE( axspec, 0 , perc , min(y[yflag], na.rm=TRUE), max(y[yflag], na.rm=TRUE) )
      }
    
    axis(2, at=raxspec, labels=format.default(axspec, digits=3), pos=min(x, na.rm=TRUE))

    if(ygrid==TRUE)
      {
      
        ##  abline(h=raxspec)
        segments(rep(min(x, na.rm=TRUE), length(raxspec)), raxspec,  rep(max(x, na.rm=TRUE), length(raxspec)) , raxspec, lty=2, col=rgb(0.2, 0.2, 0.2))

      }

    if(ylog==TRUE)
      {
        mtext(side=2, at=perc/2, text="Hz or s" , line=0)
      }
    else
      {
        mtext(side=2, at=perc/2, text="Hz" , line=2)
      }


    if(!is.null(STAMP))
      {
        
        ###   mtext(side=1, at=max(x), text=STAMP , line=2.5, adj=1)
        mtext(side=3, at=0, text=STAMP , line=1.5, adj=0)
 
      }
    
    
    
    axtrace = range(a, na.rm=TRUE)
    if(axtrace[1]< axtrace[2])
      {
        raxtrace= RPMG::RESCALE( axtrace, perc , 1.0 , axtrace[1],  axtrace[2])
        axis(4, at=raxtrace, labels=format.default(axtrace, digits=3), pos=max(tim, na.rm=TRUE))

        if( !is.null(WUNITS)  )
          {
            axis(4, at=raxtrace, labels=format.default(axtrace, digits=3), pos=max(tim, na.rm=TRUE))
            mtext(side=4, at=mean(raxtrace), text=WUNITS)
          }
        
      }
    else
      {

        message("plotevol error in input amplitudes")
      }
    

    if(CSCALE==TRUE)
      {
        if(IMAGE)
          { 
            RPMG::HOZscale( ImPlot, col, units=units, s1=0.4, s2=0.95)
          }
        
      }
    
    invisible(list(y=y[yflag], why=why, yBounds=c(0,perc, Yplmin ,  Yplmax ), x=x, yat=raxspec))

  }


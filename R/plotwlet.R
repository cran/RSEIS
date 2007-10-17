`plotwlet` <-
function(baha, Ysig, dt , zscale=1,  zbound=NULL, col=rainbow(100) , ygrid=FALSE, STAMP="")
  {
    if(missing(zscale)) { zscale = 1 }
    
    if(missing(col)) { col=rainbow(50) }
    
     if(missing(ygrid)) { ygrid=FALSE }
       if(missing(STAMP)) { STAMP=NULL }
       if(missing(zbound)) { zbound=NULL }


    perc = 0.85

    
    if(baha$flip==TRUE)
      {
        
###NO: yax = rev(baha$nvoice*((1:baha$noctave)-1)/(baha$nvoice*baha$noctave))
        yax = rev(baha$nvoice*((1:baha$noctave))/(baha$nvoice*baha$noctave))
        
        
      }
    else
      {
        
        yax =baha$nvoice*((1:baha$noctave)-1)/(baha$nvoice*baha$noctave)
      }
    
    

    a = Ysig

    ##   DSPEC = baha$img
    numfreqs = nrow(baha$img)
    y =  yax
    x = dt*(1:nrow(baha$img))
     
    yflag = rep(TRUE, length(y))

    tim = dt*seq(0, length=length(a))
   
    
  ##   image(x,why,t(DSPEC[1:(numfreqs/2),]), add=TRUE, col = col,xlab='time', ylab='freq', axes=FALSE)

   ##   image(x,why,log10(t(DSPEC[1:(numfreqs/2),])), add=TRUE, col = col,xlab='time', ylab='freq', axes=FALSE)

    IMAT =baha$img

 ##print(y)
    why   = sort( RESCALE( 1:ncol(baha$img) , 0 , perc , 1, ncol(baha$img) ))
 ##print(why)
    

    if(zscale<=1){ ImPlot = IMAT; units="Amp" } 
    if(zscale==2){ ImPlot =     log10(IMAT) ; units="Log Amp"}
    if(zscale==3){ ImPlot = sqrt(IMAT) ; units="SQRT Amp"}
    if(zscale==4){ ImPlot = 20*log10( IMAT/ max( IMAT, na.rm=TRUE) ) ; units="DB"}
       if(zscale>4){ ImPlot = IMAT; units="Amp" }  

   ##   par(mfrow=c(1,1))
    par(xaxs='i', yaxs='i')

    plot(range(tim), c(0,1), axes=FALSE, type='n', xlab='', ylab='log(scale)')

    if(!is.null(zbound))
      {
        image.default(x=x, y=why , z=ImPlot , add=TRUE, col = col, zlim=zbound , xlab='time', ylab='log(scale)', axes=FALSE)
  }
    else
      {
        image.default(x=x, y=why , z=ImPlot , add=TRUE, col = col,  xlab='time', ylab='log(scale)', axes=FALSE)

      }

    if(!is.null(baha$ridge))
      {

         image(x=x, y=why , z=baha$ridge , add=TRUE, col = tomo.colors(100))


      }
    
    trace = RESCALE( a, perc , 1.0  , min(a), max(a) )

    
   lines(tim, trace)

    ##  sy = RESCALE( a, perc  , 1.0  , min(a), max(a) )
    Tdiff = max(tim)-min(tim)
    
    ##   segments(max(tim)-Tdiff*.04-DEVOL$wpars$Ns*dt, 0.76, max(tim)-Tdiff*.04, 0.76, lwd=2)
    

                                        # axis(1)
                                        #  axis(3)
    
    xtix = pretty(x, n=10)
  #   xtix = xtix[xtix>=min(x)&xtix<=max(x)]

 #    xtix = c(floor(min(x)),xtix,  floor(max(x)))
    
    axis(3,tck=.01,at=xtix,lab=FALSE)
    mtext( side=3,    at=xtix, text=xtix, line=.5)


    axis(1,tck=.01,at=xtix,lab=FALSE)
    mtext( side=1,    at=xtix, text=xtix, line=.25)

   
                                        #  title(xlab="Time, s")
    mtext(side=1, at=max(x), text="Time, s" , line=1.5, adj=1)


    if(!is.null(STAMP))
      {
        
        ###   mtext(side=1, at=max(x), text=STAMP , line=2.5, adj=1)
        mtext(side=3, at=0, text=STAMP , line=1.5, adj=0)
 
      }
    
      

    raxspec= RESCALE(yax , 0 , perc , 0, 1 )
      
    axis(2, at=raxspec, labels=2^(1:baha$noctave))
    
   ##    axis(2, at=raxspec, labels=format.default(axspec, digits=3), pos=min(x))

    if(ygrid==TRUE)
      {
      
        ##  abline(h=raxspec)
        segments(rep(min(x), length(raxspec)), raxspec,  rep(max(x), length(raxspec)) , raxspec)

      }


  

    
    axtrace = range(a)
    raxtrace= RESCALE( axtrace, perc , 1.0 , min(a), max(a) )
    axis(4, at=raxtrace, labels=format.default(axtrace, digits=3), pos=max(tim))

    if(!is.null(zbound))
      {
        HOZscale(zbound, col, units=units, s1=0.4, s2=0.95)
      }
    else
      {
        HOZscale( ImPlot, col, units=units, s1=0.4, s2=0.95)
        
      }
    invisible(list(y=y[yflag], why=why, yBounds=c(0,perc), x=x, yat=raxspec))
    
  }


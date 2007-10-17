`evolfft` <-
function(a, dt=0, Nfft=0,  Ns=0, Nov=0, fl=0, fh=10 )
  {
    ###  Nfft=1024;Ns=250;Nov=240;fl=0; fh=10
    if(missing(dt)) { dt=1;}
    if(missing(Nfft)) { Nfft=1024;}
    if(missing(Ns)) { Ns=250;}
    if(missing(Nov)) { Nov=240;}
    if(missing(fl)) { fl=0;}
    if(missing(fh)) { fh=1/(2*dt);}

    NT = length(a);
    nyquistf = 1/(2*dt);
    if(Nov<1)
      {
        Nov = floor(Ns - 0.1*Ns);
      }
    
    Ns = floor(Ns)
    
    kcol =floor( (NT-floor(Nov) )/(Ns-floor(Nov)))

    
    if(kcol<Ns)
      {
         Ns = kcol
         Nov = floor(Ns-0.1*Ns)
         kcol =floor( (NT-floor(Nov) )/(Ns-floor(Nov)))
       }
    
    
    
    min1 = Nfft%%2;
    if(min1 == 0)
      {
        ## /* even */
        krow = (Nfft/2);
      } else {
        ##  /*  odd */
        krow = (Nfft+1)/2;
      }
    
    skiplen = Ns - Nov;
    
    df = 1.0/(Nfft*dt);
    numfreqs=krow;


   ###  print(paste(sep=' ', "evolfft kcol=", kcol, "krow=", krow, "Ns", Ns, "Nov", Nov))

     print(paste(sep=' ', "evolfft kcol=", kcol, "krow=", krow, "Ns", Ns, "Nov", Nov))
    if(kcol<1)
      {
        print(paste(sep=' ', "error in evolfft kcol=", kcol, "krow=", krow, "NT", NT, "Ns", Ns, "Nov", Nov))
        return()
      }
          
    DMAT = matrix(rep(0,krow*kcol), ncol=kcol, nrow=krow)

    m = 1:(kcol)
    ibeg=((m-1)*skiplen)+1;
     iend = floor(ibeg+Ns-1)

    

    
    #  print(cbind(ibeg, iend))
   
    
    for( i in m)
      { 
       ###  print(paste(sep=" ", m, ibeg, iend, NT))
        tem = a[ibeg[i]:iend[i]]
        tem = tem-mean(tem, na.rm=TRUE)
        tem = spec.taper(tem, p=0.05)
        tem =  c(tem,rep(0,krow-Ns))
        if(length(tem)<krow)
          {
           ### print(paste(sep=" ", m, ibeg, iend, NT));
            DMAT[,i] = rep(NA, length=krow)
          }
        else
          {
            DMAT[,i] = tem
          }
      }
    
    DFFT = mvfft(DMAT)

   DSPEC = Mod(DFFT)
     # col=heat.colors(50)


    
    x = (ibeg+Ns/2)*dt
    
    freqs = df*c(0:((numfreqs/2)-1),(-numfreqs/2):(-1)  )

    y = (1:(numfreqs/2))*2*df

  
   
    RET = list(sig=a, dt=dt, numfreqs=numfreqs, wpars=list(Nfft= Nfft,  Ns=Ns, Nov=Nov, fl=fl, fh=fh), DSPEC=DSPEC, freqs=y, tims=x)

    ## plotevol(RET)
    
    invisible(RET)

  }


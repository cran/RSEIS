`JSAC.seis` <-function(fnames, Iendian=1, HEADONLY=FALSE , BIGLONG=FALSE, PLOT=-1, RAW=FALSE)
{
  ###  get a bunch of sac files from a directory and store in structure
  ####  
  
  if(missing(PLOT)) { PLOT=-1 }
  
  if(missing(Iendian)) { Iendian=1 }
  if(missing(HEADONLY)) {HEADONLY=FALSE }
  if(missing(BIGLONG)) { BIGLONG=FALSE}
  if(missing(RAW)) { RAW=FALSE }

isign = TRUE
   
 aunits = 'volts'
  GIVE = vector(mode='list', length=length(fnames))
  
  DATAendian =c("little", "big", "swap")
 Kendian =c(1,2,3)

  #### Iendian should now be a vector


 

if(is.character(Iendian))
  {
    endianVEC =Iendian
  }
  else
    {
      endianVEC = DATAendian[match(Iendian , Kendian )]
    }
 if(length(endianVEC)<length(fnames)) { endianVEC  = rep(endianVEC, times=length(fnames) ) }


  
  for(i in 1:length(fnames))
  {
      fn =  fnames[i] 
      if(file.exists(fn)==FALSE)
        {
            warning(paste(sep=' ', "file does not exist", fn) ); 
            next;
        }
        else
        {
###  message(paste(sep=' ', "file exists", fn) );

        }

       

        ZED =    read1sac(fn , Iendian=endianVEC[i]  , HEADONLY=HEADONLY, BIGLONG=BIGLONG  )
        
####   GIVE[[i]] =  ZED
         

      dt = ZED$HEAD$delta
      thesta =   ZED$HEAD$kstnm
      thecomp = ZED$HEAD$kcmpnm
      
####  need to fix station and comp names - no blanks!
      thecomp1=  thecomp
      sblank  = unlist( strsplit(thecomp1, split="") )
      thecomp=   paste(sblank[which(sblank!=" ")], collapse="")
      
      thesta1= thesta
      sblank  = unlist( strsplit(thesta1, split="") )
      thesta=   paste(sblank[which(sblank!=" ")], collapse="")

        yr = ZED$HEAD$nzyear
        jd = ZED$HEAD$nzjday
        hr = ZED$HEAD$nzhour
        mi = ZED$HEAD$nzmin
        sec = ZED$HEAD$nzsec
        msec = ZED$HEAD$nzmsec
        sec = sec + msec/1000 +ZED$HEAD$b
        
        N =    ZED$HEAD$npts
        md = getmoday(yr, jd)
        
        t1 = 0
        t2 = dt*(N-1)
        
        tstart = list(yr=yr, jd=jd , mo=md$mo, dom=md$dom,
                      hr=hr, mi=mi, sec=sec, msec=0, dt=dt, t1=t1,
                      t2=t2, off=0)
 
        
        GIVE[[i]] =   list(fn=fn, sta=thesta,  comp=thecomp, dt=dt,
                           DATTIM=tstart, N=N, units=aunits , amp=ZED$amp, HEAD=ZED$HEAD,
                           IO=list(kind=2, Iendian=Iendian,  BIGLONG=BIGLONG)   )
        
        
        
        
    }
  invisible(GIVE)
}


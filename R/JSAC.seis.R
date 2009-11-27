`JSAC.seis` <-function(fnames, Iendian=1, HEADONLY=FALSE , BIGLONG=TRUE, PLOT=FALSE)
{
  ###  get a bunch of sac files from a directory and store in structure
  ####  
  
  if(missing(PLOT)) { PLOT=FALSE }
  
  if(missing(Iendian)) { Iendian=1 }
  if(missing(HEADONLY)) {HEADONLY=FALSE }
  if(missing(BIGLONG)) { BIGLONG=TRUE}

isign = TRUE
   
   ########  on some systems LONG = 8 bytes
   #######   on others LONG = 4 bytes
    #####   there may be other differences that
     ###   I am not aware of but this is enough for now.
  
  if(BIGLONG)
    {
      
    ishort = 2
  iint  = 4
  ilong = 8
  ifloat = 4
  idouble = 8

  }
  else
    {

    ishort = 2
  iint  = 4
  ilong = 4
  ifloat = 4
  idouble = 8



    }


  
  
  GIVE = as.list(1:length(fnames))
  
  DATAendian =c("little", "big", "swap")

  if(is.character(Iendian))
    {
      Iendian = grep(Iendian, DATAendian)
      theENDIAN = DATAendian[Iendian]
    }
  else
    {
      theENDIAN = DATAendian[Iendian]
      
    }
  
  for(i in 1:length(fnames))
    {

      fn = fnames[i]
      infile = fn
####   print(fn);
###  if this file does not exist, exit!
      if(file.exists(infile)==FALSE)
        {
          print(paste(sep=' ', "file does not exist", fn) ); 
          next;
        }
      else
        {
###  print(paste(sep=' ', "file exists", fn) );

        }
   
      zz <- file(fn, "rb")


      A1 = vector(length=70)
      for(j in 1:70)
        {
      A1[j] =  readBin(zz, numeric() , n = 1, size = ifloat, signed = isign,
        endian = theENDIAN)
    }


      A2 = vector(length=40)
      for(j in 1:40)
        {
      A2[j] =  readBin(zz, integer() , n = 1, size = ilong, signed = isign,
        endian = theENDIAN)
    }

   ## close(zz)


      
     ##  A4 =  getBINstr(zz, 8,  endian =theENDIAN)

     A4 = readChar(zz, 8, useBytes = FALSE)
      
      A5 = readChar(zz, 16, useBytes = FALSE)

      B4 = vector()
      for(k in 1:21) B4[k] = readChar(zz, 8, useBytes = FALSE)


      

      N = A2[10]
      
        ##   close(zz)
      

     ## i1 = unlist( strsplit(split=" ", B4[4]) )
      
    thecomp1=  as.character(B4[18])

      sblank  = unlist( strsplit(thecomp1, split="") )

      thecomp=   paste(sblank[which(sblank!=" ")], collapse="")

      dt = as.numeric( A1[1] )
      
      DATIM =   as.numeric(A2[1:4] )

      sec = A2[5]+ A2[6]/1000
      
      thesta1= as.character(A4)

      sblank  = unlist( strsplit(thesta1, split="") )

      thesta=   paste(sblank[which(sblank!=" ")], collapse="")

     ###   thecomp= B4[18]
      aunits="volts"
     ###  print(paste(sep=' ', infile, thesta, thecomp, aunits, N, dt, sec))
      
      md = getmoday(DATIM[2], DATIM[1])

      t1 = 0
      t2 = dt*(N-1)
      

      tstart = list(yr=DATIM[1], jd=DATIM[2] , mo=md$mo, dom=md$dom, hr=DATIM[3], mi=DATIM[4], sec=sec, msec=0, dt=dt, t1=t1,
        t2=t2, off=0)

  ###      if(is.null(thesta))   thesta="XXX"
  ###      if(is.null(thecomp))  thecomp="X"
   ###      if(is.na(thesta))   thesta="XXX"
   ###     if(is.na(thecomp))  thecomp="X"
           
      if(is.null(aunits))  aunits="volts"
     
      if(HEADONLY==TRUE)
        {
          
         ###  print(paste("headonly ", i))
          
          x = NULL
          aunits=NA
        }
      else
        {
         
          D1 = readBin(zz, numeric() , n = N , size =4,  endian = DATAendian[Iendian], signed = TRUE)
           N = length(D1)
          x = as.vector(D1)
        }

      
      close(zz)
      
      GIVE[[i]] = list(fn=fn, sta=thesta,  comp=thecomp, dt=dt, DATTIM=tstart, N=N, units=aunits , amp=x)

      
      if(PLOT==TRUE)
        {
          plot(x, type='l', main=paste(sep=' ', thesta, thecomp))
          print("left CLICK in WINDOW for NEXT TRACE:")
          locator(1)
        }
    }
  invisible(GIVE)
}


`JSEGY.seis` <-function(fnames, Iendian=1 , HEADONLY=FALSE, BIGLONG=TRUE, PLOT=FALSE)
{
  ###  get a bunch of segy files from a directory and store in structure
  ####  
  
  if(missing(PLOT)) { PLOT=FALSE }
  
  if(missing(Iendian)) { Iendian=1 }
  if(missing(HEADONLY)) {HEADONLY=FALSE }
  if(missing(BIGLONG)) { BIGLONG=TRUE}

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

      A1 =  readBin(zz, integer() , n = 7, size = iint, signed = TRUE,
        endian = theENDIAN)
      
      A2 =  readBin(zz, integer() , n = 4, size =ishort , signed = TRUE,
        endian = theENDIAN)
      
      A3 =  readBin(zz, integer() , n = 8, size = iint, signed = TRUE,
        endian = theENDIAN)

      
 ###  short elevationScale;    /*  68 Elevation Scaler: scale = 1 */
 ###   short coordScale;        /*  70 Coordinate Scaler: scale = 1 */
      A4 = readBin(zz, integer() , n = 2 , size =ishort,  endian =theENDIAN, signed = TRUE)
      

      A5 = readBin(zz, integer() , n = 4 , size =iint,  endian = theENDIAN, signed = TRUE)

      A6 = readBin(zz, integer() , n =13  , size =ishort,  endian = theENDIAN, signed = TRUE)
      coordUnits = A6[1]

      
      sampleLength=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE)
      deltaSample=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE)
      gainType=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE);        
      gainConst=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE);    


      
      initialGain =readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE);

      
      

      B6 = readBin(zz, integer() , n =16  , size =ishort,  endian = theENDIAN, signed = TRUE)


      
      year=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE)
      day=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE);
      hour=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE); 
      minute=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE);
      second=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE);

      
      timeBasisCode=readBin(zz, integer() , n = 1 , size =ishort,  endian =theENDIAN, signed = TRUE);

      B7 = readBin(zz, integer() , n =6  , size =ishort,  endian = theENDIAN, signed = TRUE)


 
      stationname=readChar(zz, 6, useBytes = FALSE)
      sensorserial=readChar(zz, 8, useBytes = FALSE)
      channelname=readChar(zz, 4, useBytes = FALSE)
      
      ##  print(paste(sep="", "<",channelname,">"))

##  close(zz)
          
      
 ## print(paste(sep=" ", "Name=", stationname,sensorserial,channelname))  

       
      totalStaticHi= readBin(zz, integer() , n = 1 , size =ishort,  endian = theENDIAN, signed = TRUE)


      samprate= readBin(zz, integer() , n = 1 , size =iint,  endian = theENDIAN, signed = TRUE)


      dataform=readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)

      msecs =readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)

     ##   print(paste(sep=" ", "time start=", year,day,hour,minute, second,  msecs  ))
      

   ##  print(paste(sep=" ", "gains", gainType, gainConst ))


    
      
      trigyear=readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)
      trigday=readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)
      trighour=readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)
      trigminute=readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)
      trigsecond=readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)
      trigmills=readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)

  ##      print(paste(sep=" ","trigtime=", trigyear,  trigday,trighour , trigminute, trigsecond, trigmills  ))
      
      scalefac=readBin(zz, numeric() , n = 1 , size =ifloat  ,  endian = theENDIAN, signed = TRUE)

      if(gainConst == 0)gainConst = 1
      if(scalefac == 0) scalefac  = 1

      

      scalefac = scalefac / gainConst;

      
      
      instno=readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)

      nottobeused =readBin(zz, integer() , n = 1 , size =ishort ,  endian = theENDIAN, signed = TRUE)
      numsamps =readBin(zz, integer() , n = 1 , size =iint ,  endian = theENDIAN, signed = TRUE)

      max =readBin(zz, integer() , n = 1 , size =iint ,  endian = theENDIAN, signed = TRUE)

      min =readBin(zz, integer() , n = 1 , size =iint ,  endian = theENDIAN, signed = TRUE)


 ##  print(paste(sep=" ", "numsamps=", numsamps, "sampleLength=", sampleLength  ))
  ##  print(paste(sep=" ",  "scalefac=",   scalefac ))
      ####################################      done reading header
      
      N = numsamps

      dt = as.numeric( deltaSample )/1000000
      
      DATIM =   c(year, day, hour, minute)

      
      sec = second+  msecs/1000000
      
      thesta= stationname
      thecomp= channelname

      
       md = getmoday(DATIM[2], DATIM[1])

      t1 = 0
      t2 = dt*(N-1)
      

      tstart = list(yr=DATIM[1], jd=DATIM[2] , mo=md$mo, dom=md$dom, hr=DATIM[3], mi=DATIM[4], sec=sec, msec=0, dt=dt, t1=t1,
        t2=t2, off=0)

      aunits="unknown"
    ####  if(is.null(thesta))   thesta="XXX"
     #### if(is.null(thecomp))  thecomp="X"
     ####  if(is.na(thesta))   thesta="XXX"
     #### if(is.na(thecomp))  thecomp="X"
           
      

      if(HEADONLY==TRUE)
        {
          
        ####   print(paste("headonly ", i))
          
          x = NULL
          aunits=NA
          
          
          
        }
      else
        {
####################################   read in floating point samples
          
          D1 = readBin(zz, integer() , n = N , size =iint ,  endian = theENDIAN, signed = TRUE)
          

          
###    i1 = unlist( strsplit(split=" ", B4[4]) )
          
          x = scalefac * as.vector(D1)
          aunits="volts"

        }
      close(zz)


      
      GIVE[[i]] = list(fn=fn, sta=thesta,  comp=thecomp, dt=dt, DATTIM=tstart, N=N, units=aunits , amp=x)

      
      if(PLOT==TRUE)
        {
          plot(x, type='l', main=paste(sep=' ', thesta, thecomp))
          print("left CLICK once in WINDOW for NEXT TRACE:")
          locator(1)
        }
    }
  invisible(GIVE)
}


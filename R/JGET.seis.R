`JGET.seis` <-
function(fnames, kind=1, PLOT=FALSE)
{
  ###  get a bunch of AH files from a directory and store in structure
  ####  kind 1=segy, 2=sac, 3=AH
  
  if(missing(PLOT)) { PLOT=FALSE }
  if(missing(kind)) { kind=1 }

  GIVE = as.list(1:length(fnames))

  ii = 1

  DATIM =  rep(0,length=4)
  n=1
  dt=0.025000
  sec = 0
  thesta="XXXXX"
  thecomp="XXXXX"


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
      
      barfa = .C("CALL_JSETSEIS",PACKAGE = "RSEIS",
        infile,
        as.integer(kind),
        as.integer(n),
        as.double(dt), 
        as.integer(DATIM),   
        as.double(sec),
        thesta , thecomp
        )
      
      
      N = barfa[[3]]
      dt = barfa[[4]]
      DATIM = barfa[[5]]
      sec = barfa[[6]]
      
      thesta=barfa[[7]]
      thecomp=barfa[[8]]

      

      if(kind==2)
        {
          if(thesta=="-12345")
            {
              stn = parseFN2STA(infile)
              thesta=stn$sta
              thecomp=stn$comp
            }
        }
      aunits="volts"
     ###  print(paste(sep=' ', infile, thesta, thecomp, aunits, N, dt, sec))

      
      x = rep(0,length=N)
      infile = fn
      

      barf = .C("CALL_JGETSEIS",PACKAGE = "RSEIS",
        infile,
        as.integer(kind),
        as.double(x),
        as.integer(n),
        as.double(dt), 
        as.integer(DATIM),   
        as.double(sec)
        )



      x = barf[[3]]
      N = barf[[4]]
      dt = barf[[5]]
      DATIM = barf[[6]]
      sec = barf[[7]]

      md = getmoday(DATIM[2], DATIM[1])

      t1 = 0
      t2 = dt*(N-1)
      

      tstart = list(yr=DATIM[1], jd=DATIM[2] , mo=md$mo, dom=md$dom, hr=DATIM[3], mi=DATIM[4], sec=sec, msec=0, dt=dt, t1=t1,
        t2=t2, off=0)


      if(is.null(thesta))   thesta="XXX"
      if(is.null(thecomp))  thecomp="X"
      
      if(is.null(aunits))  aunits="volts"
     
     
      GIVE[[i]] = list(fn=fn, sta=thesta,  comp=thecomp, dt=dt, DATTIM=tstart, N=N, units=aunits , amp=x)

      
      if(PLOT==TRUE)
        {
          plot(barf[[3]], type='l', main=paste(sep=' ', thesta, thecomp))
          print("CLICK in WINDOW for NEXT TRACE:")
          locator()
        }
    }
  invisible(GIVE)
}


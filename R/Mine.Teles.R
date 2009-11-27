Mine.Teles <-
function(DB, ev, sta, ista=1, usta="DOM", acomp="V" , velmodel=NULL, IASPpath ="", regdist=110 )
  {  #### Mine.Teles  function

    ####   program to mine a seismic data set given event locations
    ###   and station locations
     ##  each event: list(lat, lon, depth, time=list(yr, jd, hr, mi, sec) , magnitude)
    ###  stations: list(name, lat lon, elevation)

    ###  DB is a dataBase

    tpre = 5
    tpost = 5
    PLOT=TRUE
    DIAG = FALSE
    if(missing(ista)){ ista = 1:length(sta$name)   }
      
    if(missing(IASPpath)){ IASPpath = "/home/lees/Progs/TTIMES"}

    if( file.access( IASPpath, mode = 0)<0 )
      {
        print("ERROR: Need access to directory with travel time tables for tau-P")
        return(-1)
      }

    if(missing(usta)){ usta = sta$name[ista] }

    if(missing(acomp)){acomp="V"}

    if(missing(regdist)){regdist=110}
    
    if(missing(velmodel)){  ###  default velocity model (japan)
      velmodel = list(
        zp=c(0,2.5,5,10,15,20,25,32,42,50,150),
        vp=c(5.47,5.73,5.87,6.13,6.4,6.67,6.94,7.8,7.84,7.87,8),
        ep=c(0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07,0.07),
        zs=c(0,2.5,5,10,15,20,25,32,42,50,150),
        vs=c(3.24,3.37,3.43,3.56,3.69,3.82,3.94,4.41,4.43,4.44,6.44),
        es=c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1),
        name="jap1.vel",
        descriptor=c(
          "##MODEL J2  JAPAN step VELOCITY MODEL (lees , 1990)"  ,            
          "# P DEPTH   P VEL      PERR      S DEPTH    S VEL    SERR        ")
        )
      }

    if(is.character(velmodel))
      {

        velmodel = Get1Dvel(velmodel)

      }


    Nev = length(ev)
    
    for(i in 1:Nev)
      {  ##cycle through all events
        gev = ev[[i]]
        elat = gev$lat
        elon = gev$lon
        ez =   gev$z
        
        evtimejd = gev$jd+ gev$hr/24 + gev$mi/(24*60)+ (gev$sec )/(24*60*60)

        dists = distaz(elat , elon, sta$lat, sta$lon)
        evfname = filedatetime(gev, 0)
         
        nphases = 1
        arrival.times = list()
        for(j in 1:length(dists$dist))
          { #### cycle through all selected stations

            if(dists$dist[j]<regdist)
              {
                tregp = travel.time1D(dists$dist[j] , ez, 0, length(velmodel$zp), velmodel$zp, velmodel$vp) 
                tregs = travel.time1D(dists$dist[j] , ez, 0, length(velmodel$zs), velmodel$zs, velmodel$vs)
                phases = c("P", "S")
                secs = c(tregp$tt, tregs$tt)
                nphases = 2
              }
            else
              {
                ##   GD1 = GreatDist( elon, elat , sta$lon[j], sta$lat[j])
                Aout = TTteles(gev, sta, ista = j, IASPHOME = IASPpath)
####  these are for testing to make sure the program worked
             
                phases = Aout[[1]]$phase
                secs = Aout[[1]]$sec

                nphases = length(secs)
              }
            arrival.times[[j]] = list(name=sta$name[j], dist=c(dists$dist[j],
                                                          dists$del[j])  , phases = phases, secs=secs)
          }


        
        thephases= vector()
        thenames = vector()
        thetimes  = vector()
        L = 0
        for(j in 1:length(arrival.times))
          {
            tempname =  arrival.times[[j]]$name
            n = length(arrival.times[[j]]$secs)
            for(m in 1:n )
              {
                L = L+1
                thephases[L] = arrival.times[[j]]$phases[m]
                thenames[L] = tempname
                thetimes[L] = arrival.times[[j]]$secs[m]
              }
          }


        Tbegin =  min(thetimes)
        Tend = max(thetimes)
        
        ev[[i]]$evname = evfname
        ev[[i]]$arrivals = arrival.times
        ev[[i]]$Tbegin=Tbegin
        ev[[i]]$Tend = Tend

        if(PLOT)
          {


            if(nphases>5)
              {
                tpre=5*60
                tpost=5*60
              }
            else
              {

                tpre=5
                tpost=5
              }
            
            ed = EPOCHday(gev$yr, gev$jd, DB$origyr )
            
            evtimejd = ed$jday+ gev$hr/24 + gev$mi/(24*60)+ (gev$sec )/(24*60*60)
            t1 = gev$sec+thetimes
            t2 = 0

            flmin = floor( (Tbegin-tpre)/60 )
            
            at1 = evtimejd  + (Tbegin-tpre) /(24*60*60)
            at2 = at1 + (tpre+ Tend - Tbegin+tpost) /(24*3600)

            GH = Mine.seis(at1, at2, DB, usta, acomp)
            if(is.null(GH)) { next }

           
            
            wpx = setwpix(phase = thephases, col = 2, yr = ed$origyr , jd = ed$jday ,
              hr =gev$hr , mi =gev$mi , sec =  t1  , dur =t2 ,
              name =thenames , comp ="V" , dispcomp ="V" )

           ###   wpx = BKpfile2ypx(Aout[[1]])

           ###   gev$jd = ed$jday
            
            ###   HO = gwpix2ypx(wpx, LOC = gev , sta = sta$name, comp = wpx$comp[1])

            sel =   1:length(GH$STNS)
            STDLAB = c("DONE", "QUIT", "MAP", "zoom out", "zoom in", "Left", "Right",
              "restore", "Pinfo","WINFO",
              "XTR", "SPEC", "SGRAM" ,"WLET", "FILT", "UNFILT",
              "SCALE", "Postscript")
            psave = PICK.GEN(GH,sel =sel,  APIX=wpx, STDLAB = STDLAB )
            
            ev[[i]]$PICKOUT  = psave

            if(psave$PUSHED[length(psave$PUSHED)] =="QUIT")
              {
                return(ev)
              }
          }

      }

    invisible(ev)
    
  }


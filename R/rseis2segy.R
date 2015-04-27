rseis2segy<-function(GH, sel=1, win=c(0,1), path=".", BIGLONG=FALSE )
  {
##############  convert an RSEIS structure to SEGY format
########   if dir is provided, the individual files will be
#########    written in that directory
###  rseis2segy(GH, sel = which(GH$STNS=="FAR" & GH$COMPS %in% c("V", "N", "E")),
    ###################    path = ".", BIGLONG=FALSE)
    SEGYhead.names = c("lineSeq", "reelSeq", "event_number", "channel_number",
      "energySourcePt", "cdpEns", "traceInEnsemble", "traceID",
      "vertSum", "horSum", "dataUse", "sourceToRecDist",
      "recElevation", "sourceSurfaceElevation", "sourceDepth",
      "datumElevRec", "datumElevSource",
      "sourceWaterDepth","recWaterDepth",
      "elevationScale", "coordScale",
      "sourceLongOrX", "sourceLatOrY","recLongOrX", "recLatOrY",
      "coordUnits", "weatheringVelocity", "subWeatheringVelocity",
      "sourceUpholeTime", "recUpholeTime", "sourceStaticCor",
      "recStaticCor", "totalStatic", "lagTimeA", "lagTimeB",
      "delay", "muteStart", "muteEnd",
      "sampleLength", "deltaSample", "gainType", "gainConst",
      "initialGain",
      "correlated", "sweepStart", "sweepEnd", "sweepLength",
      "sweepType", "sweepTaperAtStart", "sweepTaperAtEnd",
      "taperType", "aliasFreq", "aliasSlope",
      "notchFreq","notchSlope", "lowCutFreq", "hiCutFreq",
      "lowCutSlope", "hiCutSlope",
      
      "year", "day", "hour", "minute", "second", "timeBasisCode",
      
      "traceWeightingFactor", "phoneRollPos1", "phoneFirstTrace",
      "phoneLastTrace", "gapSize", "taperOvertravel",
      
      "station_name", "sensor_serial", "channel_name",
      "totalStaticHi", "samp_rate", "data_form", "m_secs",
      "trigyear", "trigday", "trighour", "trigminute", "trigsecond",
      "trigmills", "scale_fac", "inst_no", "not_to_be_used",
      "num_samps", "max", "min")

    initsegy<-function()
      {

        N = length(SEGYhead.names)
        j = rep(0, N)
        HEAD=as.list(j)
        names(HEAD) = SEGYhead.names
        HEAD[72] = "STAXX"
        HEAD[73] ="12345"
        HEAD[74] = "XXX"

        h = list(HEAD=HEAD, amp=0)

        return(h)
      }

    
    
    if(missing(sel)) { sel = 1:length(GH$STNS) }

    
    theENDIAN =  .Platform$endian

    aunits = "volts"
    OLDir = getwd()
    RDT = rangedatetime(GH$info)
    newdir  = paste(path,filedatetime(RDT$min), sep="/")

    if(file.exists(newdir))
      {
        setwd(newdir)
      }
    else
      {
        tdir = dir.create(newdir, recursive=TRUE )
        if(tdir==TRUE) { setwd(newdir)  }
        else
          {
            print("ERROR: CANNOT create or write in this directory")
          }
      }


    ############# for back compatability, need something in here if its missing
     if(is.null(  GH$info$scalefac )) GH$info$scalefac = rep(1, times = length(GH$info$sec) )
     if(is.null(  GH$info$gain ))     GH$info$gain=   rep(1, times = length(GH$info$sec) )
    
    for(j in 1:length(sel))
      {
        i = sel[j]
        
        fn = "GH"
        thesta = GH$STNS[i]
        thecomp = GH$COMPS[i]
        dt = GH$dt[i]
        
        tstart = list(yr=GH$info$yr[i]  ,
          jd=GH$info$jd[i]  ,
          mo=GH$info$mo[i]  ,
          dom=GH$info$dom[i]  ,
          hr=GH$info$hr[i]  ,
          mi=GH$info$mi[i]  ,
          sec=GH$info$sec[i]  ,
          msec=GH$info$msec[i]  ,
          dt=GH$info$dt[i]  ,
          t1=GH$info$t1[i]  ,
          t2=GH$info$t2[i]  ,
          off=GH$info$off[i]  
          )
        N = GH$info$n[i]

        sig = GH$JSTR[[i]]
        
        ##  a1 = list(fn=fn, sta=thesta,  comp=thecomp, dt=dt, DATTIM=tstart,
        ##     N=N, units=aunits , amp=sig , IO=list(kind=2, Iendian=theENDIAN,  BIGLONG=BIGLONG))

        
        a1 =  initsegy()

###  cat(paste("a1$HEAD$",SEGYhead.names,"=",  sep="")  , sep="\n")

        a1$HEAD$lineSeq=i
        a1$HEAD$year=GH$info$yr[i]
        a1$HEAD$day=GH$info$jd[i] 
        a1$HEAD$hour=GH$info$hr[i]
        a1$HEAD$minute=GH$info$mi[i]
        a1$HEAD$second=GH$info$sec[i]
        a1$HEAD$gainConst= 1
        a1$HEAD$station_name=thesta
        a1$HEAD$sensor_serial="12345"
        a1$HEAD$channel_name=thecomp
        a1$HEAD$channel_number=thecomp
        a1$HEAD$inst_no=thesta
        
        a1$HEAD$samp_rate=dt*1000000
        a1$HEAD$m_secs=GH$info$msec[i]

        if(is.null(GH$info$scalefac[i])|is.na(GH$info$scalefac[i]) )GH$info$scalefac[i]=1
        if(is.null(GH$info$gain[i] )|is.na(GH$info$gain[i] )   )GH$info$gain[i]=1
        
        a1$HEAD$scale_fac=GH$info$scalefac[i]
        a1$HEAD$num_samps=N
        
        a1$HEAD$gainConst = GH$info$gain[i]

        
        scalefac =  GH$info$scalefac[i] / GH$info$gain[i]


        Kounts = sig*scalefac

        a1$amp = Kounts 

        segyfn = paste(sep=".",
          formatC(GH$info$yr[i] , width = 4, flag = "0") ,
          formatC(GH$info$jd[i], width = 3, flag = "0"),
          formatC(GH$info$hr[i], width = 2, flag = "0") ,
          formatC(GH$info$mi[i], width = 2, flag = "0") ,
          formatC(GH$info$sec[i], width = 2, flag = "0")  ,
          thesta ,
          thecomp,
          "SEGY")


        print(segyfn)

        ######################  write out the SEGY file
        write1segy(a1, fn=segyfn , BIGLONG=BIGLONG  )
        
      }



    
    setwd(  OLDir)
    
  }

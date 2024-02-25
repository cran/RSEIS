infoDB<-function(DB, verbose=TRUE)
  {

    if(any(DB$yr>2100))
      {

        warning("Problems with DB....need to check")
        ww = which(DB$yr>2100)
        message(ww)


      }
    Month.Names = toupper( c("Jan", "Feb", "Mar", "Apr", "May", "Jun", 
        "Jul", "Aug", "Sep", "Oct", "Nov", "Dec") )

    usta = unique(DB$sta)
    ucomp = unique(DB$comp)
    times = range(c(DB$t1, DB$t2))

    RD = rangedatetime(DB)
    D1 = showdatetime(RD[[1]], verbose=FALSE)
    D2 = showdatetime(RD[[2]], verbose=FALSE)

    DOM1 = RSEIS::getmoday(RD[[1]]$jd, RD[[1]]$yr)
    mo1 = Month.Names[DOM1$mo]
    day1 = DOM1$dom


    DOM2 = RSEIS::getmoday(RD[[2]]$jd, RD[[2]]$yr)
    mo2 = Month.Names[DOM2$mo]
    day2 = DOM2$dom

   Acal1 =  paste(RD[[1]]$yr,  mo1,  day1, sep="-")
   Acal2 =  paste(RD[[2]]$yr,  mo2,  day2, sep="-")

    jtim1 = paste(RD[[1]]$hr,  RD[[1]]$mi,  RD[[1]]$sec, sep=":")
    jtim2 = paste(RD[[2]]$hr,  RD[[2]]$mi,  RD[[2]]$sec, sep=":")

    CAL1 = paste(Acal1, jtim1, sep=' ')
    CAL2 = paste(Acal2, jtim2, sep=' ')

  dat1 =  paste(formatC(RD[[1]]$yr, width=4),
            formatC(DOM1$mo, width=2, flag=0),
            formatC(day1, width=2, flag=0), sep="-")

        tim1 = paste(formatC(RD[[1]]$hr, width=2, flag=0),
            formatC(RD[[1]]$mi, width=2, flag=0),
            formatC(RD[[1]]$sec, width=2, flag=0), sep=":")

    startsec <- as.POSIXct(strptime(paste(dat1, tim1,sep=' '),format="%Y-%m-%d %H:%M:%S", tz="GMT")  )



 dat2 =  paste(formatC(RD[[2]]$yr, width=4),
            formatC(DOM2$mo, width=2, flag=0),
            formatC(day2, width=2, flag=0), sep="-")

        tim2 = paste(formatC(RD[[2]]$hr, width=2, flag=0),
            formatC(RD[[2]]$mi, width=2, flag=0),
            formatC(RD[[2]]$sec, width=2, flag=0), sep=":")

   endsec <- as.POSIXct(paste(dat2, tim2, sep=' '),tz="GMT")


    
    
    
    if(verbose==TRUE)
    {
        cat("Unique Stations:\n")
    cat(usta, sep=" "); cat("\n")
    cat("Unique Components:\n")
    cat(ucomp, sep=" "); cat("\n")
    cat("Times(jd):\n")
    cat(times, sep=" "); cat("\n")

        cat("Dates 1:\n")
        cat(D1, sep="\n")
        cat(D2, sep="\n")
        cat('####', sep="\n")
        cat("Dates Gregorian:\n")
        cat(CAL1, sep="\n")
        cat(CAL2, sep="\n")


        
    }
    

   ##  message(D1)
   ##  message(D2)
   
    invisible(list(usta=usta, ucomp=ucomp, start=D1,
                   end=D2, at1=times[1], at2=times[2],YR1=RD[[1]]$yr, YR2=RD[[2]]$yr, CAL1=CAL1, CAL2=CAL2, startsec=startsec, endsec=endsec ))


  }

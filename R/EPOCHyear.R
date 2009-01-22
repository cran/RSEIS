EPOCHyear<-function(iday,  origyr=1972)
  {
    if(missing(origyr)) { origyr=1972 }

    N = length(iday)
    
    IYEARS = rep(NA, N)
    IJD = rep(NA, N)


   #  print(iday)
  #    print(origyr)

    for(i in 1:N)
      {
        
        itemp = trunc(iday[i]/365)+5

        if(iday[i]<0) {
          IYEARS[i] = origyr
          IJD[i]  = 0
          next
        }

        
        ii = seq(from=origyr, to=origyr+itemp, by=1)
    
        YRDAYS =  c(1, DAYSperYEAR(ii))
        
        csi = cumsum(YRDAYS)

        
        i2 = findInterval(iday[i], csi)

        if(iday[i]==DAYSperYEAR(origyr))
          {
            i2=1
          }
        

        IYEARS[i] = ii[i2]

        theday = EPOCHday(IYEARS[i] ,jd=1, origyr=origyr)

        IJD[i]  =  iday[i] - theday$jday +1 

        
      }
    
   
    return(list(yr=IYEARS, jd=IJD) )

  }

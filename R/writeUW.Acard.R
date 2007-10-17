`writeUW.Acard` <-
function(LOC)
  {
    ## A 200406270357 30.03  0N4156  77W5069 12.00  4.1  0/000   0  0 0.00  0.0XX EC
    ############ writeUW.Acard(P$LOC)
    
      ID = paste(sep="",
        formatC(LOC$yr, format="d", wid=4, flag="0"),
        formatC(LOC$mo, format="d", wid=2, flag="0"), 
	formatC(LOC$dom, format="d", wid=2, flag="0"), 
	formatC(LOC$hr, format="d", wid=2,  flag="0"), 
	formatC(LOC$mi, format="d", wid=2,flag="0"))

      L = abs(LOC$lat)
      LAT1 = floor(L)
      LAT2 = round((L - LAT1)*6000)
      if(LOC$lat<0){LATNS="S"}  else  {LATNS="N"}
          
      L = abs(LOC$lon)
      LON1 = floor(L)
      LON2 = round((L - LON1)*6000)
      if(LOC$lon<0) {LONEW="W"} else  {LONEW="E"}
          
      if(is.na(LOC$mag)) { LOC$mag=0 }
      if(is.na(LOC$z)) { LOC$z=0 }

### print(paste(sep=' ', LAT1, LATNS, LAT2, LON1, LONEW, LON2))
     
    AC = paste(sep='', "A ", ID, sprintf(fmt="%6.2f",LOC$se) , " ",
        sprintf(fmt="%02d", LAT1)   ,LATNS, sprintf(fmt="%04d", LAT2) ," ",
        sprintf(fmt="%03d",LON1),LONEW,  sprintf(fmt="%04d",LON2),
        sprintf(fmt="%6.2f",LOC$z), " ",
       sprintf(fmt="%4.1f", LOC$mag),
          "  0/000   0  0 0.00  0.0XX EC"
        )


      

      return(AC)

  }


`getEcard` <-
function(ECARD)
  {
    E = list()
    
    E$LOC = as.character(substr(ECARD, 2+1, 2+2))
    E$rms = as.numeric(substr(ECARD, 4+1, 4+6))
    E$meanres = as.numeric(substr(ECARD, 10+1, 10+6))
    E$sdres = as.numeric(substr(ECARD, 16+1, 16+6))
    E$sdmean = as.numeric(substr(ECARD, 22+1, 22+6))
    E$sswres = as.numeric(substr(ECARD, 28+1, 28+8))
    E$ndf = as.numeric(substr(ECARD, 37+1, 37+3))
    E$fixflgs= as.character(substr(ECARD, 41+1, 41+4))

    E$sterrx= as.numeric(substr(ECARD, 45+1, 45+5))
    E$sterry= as.numeric(substr(ECARD, 50+1, 50+5))
    E$sterrz = as.numeric(substr(ECARD, 55+1,  55+5))     
    E$sterrt= as.numeric(substr(ECARD, 60+1, 60+5))

    ##  these might be non-numeric
    E$mag  = as.numeric(substr(ECARD, 65+1, 65+5))    
    E$sterrmag= as.numeric(substr(ECARD, 70+1, 70+5))
 
    return(E)

  }


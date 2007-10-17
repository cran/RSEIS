`writeUW.Ecard` <-
function(E)
      {
       ecard = sprintf("E %2s%6.3f%6.3f%6.3f%6.3f%8.2f %3d%4s %5.2f%5.2f%5.2f%5.2f%5.2f%5.2f", E$LOC,  E$rms ,  E$meanres,  E$sdres, E$sdmean,  E$sswres,  E$ndf, E$fixflgs, E$sterrx, E$sterry, E$sterrz, E$sterrt, E$mag,  E$sterrmag )
       return(ecard)
      }


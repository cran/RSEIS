`writeUW.Hcard` <-
function(H)
  {
   hcard =  paste(sep=" ", "H", H$yr, H$mo, H$dom, H$hr, H$mi, H$sec, H$lat, H$lon, H$z, H$mag) 
   return(hcard)
  }

